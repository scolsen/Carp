module Primitives where

import Control.Monad (unless, when, foldM)
import Control.Monad.IO.Class (liftIO)
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import ColorText
import Commands
import Deftype
import Emit
import Lookup
import Obj
import Sumtypes
import TypeError
import Types
import Util

import Debug.Trace

found ctx binder =
  liftIO $ do putStrLnWithColor White (show binder)
              return (ctx, dynamicNil)

makePrim :: String -> Int -> String -> String -> Primitive -> (String, Binder)
makePrim name arity doc example callback =
  makePrim' name (Just arity) doc example callback

makeVarPrim :: String -> String -> String -> Primitive -> (String, Binder)
makeVarPrim name doc example callback =
  makePrim' name Nothing doc example callback

argumentErr :: Context -> String -> String -> String -> XObj -> IO (Context, Either EvalError XObj)
argumentErr ctx fun ty number actual =
  return (evalError ctx (
            "`" ++ fun ++ "` expected " ++ ty ++ " as its " ++ number ++
            " argument, but got `" ++ pretty actual ++ "`") (info actual))

makePrim' :: String -> Maybe Int -> String -> String -> Primitive -> (String, Binder)
makePrim' name maybeArity docString example callback =
  let path = SymPath [] name
      prim = XObj (Lst [ XObj (Primitive (PrimitiveFunction wrapped)) (Just dummyInfo) Nothing
                       , XObj (Sym path Symbol) Nothing Nothing
                       ])
            (Just dummyInfo) (Just DynamicTy)
      meta = MetaData (Map.insert "doc" (XObj (Str doc) Nothing Nothing) Map.empty)
  in (name, Binder meta prim)
  where wrapped =
          case maybeArity of
            Just a ->
              \x c l ->
                let ll = length l
                in (if ll /= a then err x c a ll else callback x c l)
            Nothing -> callback
        err :: XObj -> Context -> Int -> Int -> IO (Context, Either EvalError XObj)
        err x ctx a l =
          return (evalError ctx (
            "The primitive `" ++ name ++ "` expected " ++ show a ++
            " arguments, but got " ++ show l ++ ".\n\n" ++ exampleUsage) (info x))
        doc = docString ++ "\n\n" ++ exampleUsage
        exampleUsage = "Example Usage\n```\n" ++ example ++ "\n```\n"

primitiveFile :: Primitive
primitiveFile x@(XObj _ i t) ctx [] =
  case i of
    Just info -> return (ctx, Right (XObj (Str (infoFile info)) i t))
    Nothing ->
      return (evalError ctx ("No information about object " ++ pretty x) (info x))
primitiveFile x@(XObj _ i t) ctx [XObj _ mi _] =
  case mi of
    Just info -> return (ctx, Right (XObj (Str (infoFile info)) i t))
    Nothing ->
      return (evalError ctx ("No information about object " ++ pretty x) (info x))
primitiveFile x@(XObj _ i t) ctx args =
  return (
    evalError ctx
      ("`file` expected 0 or 1 arguments, but got " ++ show (length args))
      (info x))

primitiveLine :: Primitive
primitiveLine x@(XObj _ i t) ctx [] =
  case i of
    Just info -> return (ctx, Right (XObj (Num IntTy (fromIntegral (infoLine info))) i t))
    Nothing ->
      return (evalError ctx ("No information about object " ++ pretty x) (info x))
primitiveLine x@(XObj _ i t) ctx [XObj _ mi _] =
  case mi of
    Just info -> return (ctx, Right (XObj (Num IntTy (fromIntegral (infoLine info))) i t))
    Nothing ->
      return (evalError ctx ("No information about object " ++ pretty x) (info x))
primitiveLine x@(XObj _ i t) ctx args =
  return (
    evalError ctx
      ("`line` expected 0 or 1 arguments, but got " ++ show (length args))
      (info x))

primitiveColumn :: Primitive
primitiveColumn x@(XObj _ i t) ctx [] =
  case i of
    Just info -> return (ctx, Right (XObj (Num IntTy (fromIntegral (infoColumn info))) i t))
    Nothing ->
      return (evalError ctx ("No information about object " ++ pretty x) (info x))
primitiveColumn x@(XObj _ i t) ctx [XObj _ mi _] =
  case mi of
    Just info -> return (ctx, Right (XObj (Num IntTy (fromIntegral (infoColumn info))) i t))
    Nothing ->
      return (evalError ctx ("No information about object " ++ pretty x) (info x))
primitiveColumn x@(XObj _ i t) ctx args =
  return (
    evalError ctx
      ("`column` expected 0 or 1 arguments, but got " ++ show (length args))
      (info x))

registerInInterfaceIfNeeded :: Context -> SymPath -> Ty -> Either String Context
registerInInterfaceIfNeeded ctx path@(SymPath _ name) definitionSignature =
  let typeEnv = getTypeEnv (contextTypeEnv ctx)
  in case lookupInEnv (SymPath [] name) typeEnv of
       Just (_, Binder _ (XObj (Lst [XObj (Interface interfaceSignature paths) ii it, isym]) i t)) ->
         if checkKinds interfaceSignature definitionSignature
           then if areUnifiable interfaceSignature definitionSignature
                then let updatedInterface = XObj (Lst [XObj (Interface interfaceSignature (addIfNotPresent path paths)) ii it, isym]) i t
                     in  return $ ctx { contextTypeEnv = TypeEnv (extendEnv typeEnv name updatedInterface) }
                else Left ("[INTERFACE ERROR] " ++ show path ++ " : " ++ show definitionSignature ++
                           " doesn't match the interface signature " ++ show interfaceSignature)
           else Left ("[INTERFACE ERROR] " ++ show path ++ ":" ++ " One or more types in the interface implementation " ++ show definitionSignature ++ " have kinds that do not match the kinds of the types in the interface signature " ++ show interfaceSignature ++ "\n" ++ "Types of the form (f a) must be matched by constructor types such as (Maybe a)")
       Just (_, Binder _ x) ->
         error ("A non-interface named '" ++ name ++ "' was found in the type environment: " ++ show x)
       Nothing -> return ctx

-- | Ensure that a 'def' / 'defn' has registered with an interface (if they share the same name).
registerDefnOrDefInInterfaceIfNeeded :: Context -> XObj -> Either String Context
registerDefnOrDefInInterfaceIfNeeded ctx xobj =
  case xobj of
    XObj (Lst [XObj (Defn _) _ _, XObj (Sym path _) _ _, _, _]) _ (Just t) ->
      -- This is a function, does it belong to an interface?
      registerInInterfaceIfNeeded ctx path t
    XObj (Lst [XObj (Deftemplate _) _ _, XObj (Sym path _) _ _]) _ (Just t) ->
      -- Templates should also be registered.
      registerInInterfaceIfNeeded ctx path t
    XObj (Lst [XObj Def _ _, XObj (Sym path _) _ _, _]) _ (Just t) ->
      -- Global variables can also be part of an interface
      registerInInterfaceIfNeeded ctx path t
    _ -> return ctx

define :: Bool -> Context -> XObj -> IO Context
define hidden ctx@(Context globalEnv _ typeEnv _ proj _ _ _) annXObj =
  let previousType =
        case lookupInEnv (getPath annXObj) globalEnv of
          Just (_, Binder _ found) -> ty found
          Nothing -> Nothing
      previousMeta = existingMeta globalEnv annXObj
      adjustedMeta = if hidden
                     then previousMeta { getMeta = Map.insert "hidden" trueXObj (getMeta previousMeta) }
                     else previousMeta
      fppl = projectFilePathPrintLength proj
  in case annXObj of
       XObj (Lst (XObj (Defalias _) _ _ : _)) _ _ ->
         return (ctx { contextTypeEnv = TypeEnv (envInsertAt (getTypeEnv typeEnv) (getPath annXObj) (Binder adjustedMeta annXObj)) })
       XObj (Lst (XObj (Deftype _) _ _ : _)) _ _ ->
         return (ctx { contextTypeEnv = TypeEnv (envInsertAt (getTypeEnv typeEnv) (getPath annXObj) (Binder adjustedMeta annXObj)) })
       XObj (Lst (XObj (DefSumtype _) _ _ : _)) _ _ ->
         return (ctx { contextTypeEnv = TypeEnv (envInsertAt (getTypeEnv typeEnv) (getPath annXObj) (Binder adjustedMeta annXObj)) })
       _ ->
         do when (projectEchoC proj) $
              putStrLn (toC All (Binder emptyMeta annXObj))
            case previousType of
              Just previousTypeUnwrapped ->
                unless (areUnifiable (forceTy annXObj) previousTypeUnwrapped) $
                  do putStrWithColor Blue ("[WARNING] Definition at " ++ prettyInfoFromXObj annXObj ++ " changed type of '" ++ show (getPath annXObj) ++
                                           "' from " ++ show previousTypeUnwrapped ++ " to " ++ show (forceTy annXObj))
                     putStrLnWithColor White "" -- To restore color for sure.
              Nothing -> return ()
            case registerDefnOrDefInInterfaceIfNeeded ctx annXObj of
              Left err ->
                do case contextExecMode ctx of
                     Check -> let fppl = projectFilePathPrintLength (contextProj ctx)
                              in  putStrLn (machineReadableInfoFromXObj fppl annXObj ++ " " ++ err)
                     _ -> putStrLnWithColor Red err
                   return ctx
              Right ctx' ->
                return (ctx' { contextGlobalEnv = envInsertAt globalEnv (getPath annXObj) (Binder adjustedMeta annXObj) })

primitiveRegisterType :: Primitive
primitiveRegisterType _ ctx [XObj (Sym (SymPath [] t) _) _ _] =
  primitiveRegisterTypeWithoutFields ctx t Nothing
primitiveRegisterType _ ctx [x] =
  return (evalError ctx ("`register-type` takes a symbol, but it got " ++ pretty x) (info x))
primitiveRegisterType _ ctx [XObj (Sym (SymPath [] t) _) _ _, XObj (Str override) _ _] =
  primitiveRegisterTypeWithoutFields ctx t (Just override)
primitiveRegisterType _ ctx [x@(XObj (Sym (SymPath [] t) _) _ _), (XObj (Str override) _ _), members] =
  primitiveRegisterTypeWithFields ctx x t (Just override) members
primitiveRegisterType _ ctx [x@(XObj (Sym (SymPath [] t) _) _ _), members] =
  primitiveRegisterTypeWithFields ctx x t Nothing members
primitiveRegisterType _ ctx _ =
  return (evalError ctx (
    "I don't understand this usage of `register-type`.\n\n" ++
    "Valid usages :\n" ++
    "  (register-type Name)\n" ++
    "  (register-type Name [field0 Type, ...])") Nothing)


primitiveRegisterTypeWithoutFields :: Context -> String -> (Maybe String) -> IO (Context, Either EvalError XObj)
primitiveRegisterTypeWithoutFields ctx t override = do
  let pathStrings = contextPath ctx
      typeEnv = contextTypeEnv ctx
      path = SymPath pathStrings t
      typeDefinition = XObj (Lst [XObj (ExternalType override) Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]) Nothing (Just TypeTy)
  return (ctx { contextTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) t typeDefinition) }, dynamicNil)

primitiveRegisterTypeWithFields :: Context -> XObj -> String -> (Maybe String) -> XObj -> IO (Context, Either EvalError XObj)
primitiveRegisterTypeWithFields ctx x t override members = do
  let pathStrings = contextPath ctx
      globalEnv = contextGlobalEnv ctx
      typeEnv = contextTypeEnv ctx
      path = SymPath pathStrings t
      preExistingModule = case lookupInEnv (SymPath pathStrings t) globalEnv of
                            Just (_, Binder _ (XObj (Mod found) _ _)) -> Just found
                            _ -> Nothing
  case bindingsForRegisteredType typeEnv globalEnv pathStrings t [members] Nothing preExistingModule of
    Left err -> return (makeEvalError ctx (Just err) (show err) (info x))
    Right (typeModuleName, typeModuleXObj, deps) -> do
      let typeDefinition = XObj (Lst [XObj (ExternalType override) Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]) Nothing (Just TypeTy)
          ctx' = (ctx { contextGlobalEnv = envInsertAt globalEnv (SymPath pathStrings typeModuleName) (Binder emptyMeta typeModuleXObj)
                      , contextTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) t typeDefinition)
                      })
      contextWithDefs <- liftIO $ foldM (define True) ctx' deps
      return (contextWithDefs, dynamicNil)


notFound :: Context -> XObj -> SymPath -> IO (Context, Either EvalError XObj)
notFound ctx x path =
  return (evalError ctx ("I can’t find the symbol `" ++ show path ++ "`") (info x))

primitiveInfo :: Primitive
primitiveInfo _ ctx [target@(XObj (Sym path@(SymPath _ name) _) _ _)] = do
  let env = contextEnv ctx
      typeEnv = contextTypeEnv ctx
  case path of
    SymPath [] _ ->
      -- First look in the type env, then in the global env:
      case lookupInEnv path (getTypeEnv typeEnv) of
        Nothing -> printer env True True (lookupInEnv path env)
        found -> do printer env True True found -- this will print the interface itself
                    printer env True False (lookupInEnv path env)-- this will print the locations of the implementers of the interface
    qualifiedPath ->
      case lookupInEnv path env of
        Nothing -> notFound ctx target path
        found -> printer env False True found
  where printer env allowLookupInALL errNotFound binderPair = do
          let proj = contextProj ctx
          case binderPair of
           Just (_, binder@(Binder metaData x@(XObj _ (Just i) _))) ->
             do liftIO $ putStrLn (show binder ++ "\nDefined at " ++ prettyInfo i)
                printDoc metaData proj x
           Just (_, binder@(Binder metaData x)) ->
             do liftIO $ print binder
                printDoc metaData proj x
           Nothing | allowLookupInALL ->
            case multiLookupALL name env of
                [] -> if errNotFound then notFound ctx target path else
                        return (ctx,  dynamicNil)
                binders -> do liftIO $
                                mapM_
                                  (\ (env, binder@(Binder metaData x@(XObj _ i _))) ->
                                     case i of
                                         Just i' -> do
                                          putStrLnWithColor White
                                                    (show binder ++ "\nDefined at " ++ prettyInfo i')
                                          _ <- printDoc metaData proj x
                                          return ()
                                         Nothing -> putStrLnWithColor White (show binder))
                                  binders
                              return (ctx, dynamicNil)
                   | errNotFound -> notFound ctx target path
                   | otherwise -> return (ctx, dynamicNil)
        printDoc metaData proj x = do
          case Map.lookup "doc" (getMeta metaData) of
            Just (XObj (Str val) _ _) -> liftIO $ putStrLn ("Documentation: " ++ val)
            Nothing -> return ()
          liftIO $ when (projectPrintTypedAST proj) $ putStrLnWithColor Yellow (prettyTyped x)
          return (ctx, dynamicNil)
primitiveInfo _ ctx [notName] =
  argumentErr ctx "info" "a name" "first" notName

dynamicOrMacroWith :: Context -> (SymPath -> [XObj]) -> Ty -> String -> XObj -> IO (Context, Either EvalError XObj)
dynamicOrMacroWith ctx producer ty name body = do
  let pathStrings = contextPath ctx
      globalEnv = contextGlobalEnv ctx
      path = SymPath pathStrings name
      elem = XObj (Lst (producer path)) (info body) (Just ty)
      meta = existingMeta globalEnv elem
  return (ctx { contextGlobalEnv = envInsertAt globalEnv path (Binder meta elem) }, dynamicNil)

primitiveType :: Primitive
primitiveType _ ctx [x@(XObj (Sym path@(SymPath [] name) _) _ _)] = do
  let env = contextGlobalEnv ctx
  case lookupInEnv path env of
    Just (_, binder) ->
      found ctx binder
    Nothing ->
      case multiLookupALL name env of
        [] ->
          notFound ctx x path
        binders ->
          liftIO $ do mapM_ (\(env, binder) -> putStrLnWithColor White (show binder)) binders
                      return (ctx, dynamicNil)
primitiveType _ ctx [x@(XObj (Sym qualifiedPath _) _ _)] = do
  let env = contextGlobalEnv ctx
  case lookupInEnv qualifiedPath env of
    Just (_, binder) -> found ctx binder
    Nothing -> notFound ctx x qualifiedPath
primitiveType _ ctx [x] =
  return (evalError ctx ("Can't get the type of non-symbol: " ++ pretty x) (info x))

primitiveMembers :: Primitive
primitiveMembers _ ctx [target] = do
  let env = contextEnv ctx
      typeEnv = contextTypeEnv ctx
      fppl = projectFilePathPrintLength (contextProj ctx)
  case bottomedTarget env target of
        XObj (Sym path@(SymPath _ name) _) _ _ ->
           case lookupInEnv path (getTypeEnv typeEnv) of
             Just (_, Binder _ (XObj (Lst [
               XObj (Deftype structTy) Nothing Nothing,
               XObj (Sym (SymPath pathStrings typeName) Symbol) Nothing Nothing,
               XObj (Arr members) _ _]) _ _))
               ->
                 return (ctx, Right (XObj (Arr (map (\(a, b) -> XObj (Lst [a, b]) Nothing Nothing) (pairwise members))) Nothing Nothing))
             Just (_, Binder _ (XObj (Lst (
               XObj (DefSumtype structTy) Nothing Nothing :
               XObj (Sym (SymPath pathStrings typeName) Symbol) Nothing Nothing :
               sumtypeCases)) _ _))
               ->
                 return (ctx, Right (XObj (Arr (concatMap getMembersFromCase sumtypeCases)) Nothing Nothing))
               where getMembersFromCase :: XObj -> [XObj]
                     getMembersFromCase (XObj (Lst members) _ _) =
                       map (\(a, b) -> XObj (Lst [a, b]) Nothing Nothing) (pairwise members)
                     getMembersFromCase x@(XObj (Sym sym _) _ _) =
                       [XObj (Lst [x, XObj (Arr []) Nothing Nothing]) Nothing Nothing]
                     getMembersFromCase (XObj x _ _) =
                       error ("Can't handle case " ++ show x)
             _ ->
               return (evalError ctx ("Can't find a struct type named '" ++ name ++ "' in type environment") (info target))
        _ -> return (evalError ctx ("Can't get the members of non-symbol: " ++ pretty target) (info target))
  where bottomedTarget env target =
          case target of
            XObj (Sym targetPath _) _ _ ->
              case lookupInEnv targetPath env of
                -- this is a trick: every type generates a module in the env;
                -- we’re special-casing here because we need the parent of the
                -- module
                Just (_, Binder _ (XObj (Mod _) _ _)) -> target
                -- if we’re recursing into a non-sym, we’ll stop one level down
                Just (_, Binder _ x) -> bottomedTarget env x
                _ -> target
            _ -> target

-- | Set meta data for a Binder
primitiveMetaSet :: Primitive
primitiveMetaSet _ ctx [target@(XObj (Sym path@(SymPath _ name) _) _ _), XObj (Str key) _ _, value] = do
  let env = contextGlobalEnv ctx
      pathStrings = contextPath ctx
      fppl = projectFilePathPrintLength (contextProj ctx)
      fullPath = consPath pathStrings path
  case lookupInEnv fullPath env of
    Just (foundEnv, binder@(Binder _ xobj)) ->
      -- | Set meta on existing binder
      setMetaOn ctx (Just foundEnv) binder
    Nothing ->
      -- | Try dynamic scope
      case lookupInEnv (consPath ["Dynamic"] fullPath) env of
        Just (foundEnv, binder@(Binder _ xobj)) ->
          setMetaOn ctx (Just foundEnv) binder
        Nothing ->
          case path of
            -- | If the path is unqualified, create a binder and set the meta on that one. This enables docstrings before function exists.
            (SymPath [] name) ->
              setMetaOn ctx Nothing (Binder emptyMeta (XObj (Lst [XObj DocStub Nothing Nothing,
                                                                  XObj (Sym (SymPath pathStrings name) Symbol) Nothing Nothing])
                                                       (Just dummyInfo)
                                                       (Just (VarTy "a"))))
            (SymPath _ _) ->
              return (evalError ctx ("`meta-set!` failed, I can't find the symbol `" ++ show path ++ "`") (info target))
    where
      setMetaOn :: Context -> Maybe Env -> Binder -> IO (Context, Either EvalError XObj)
      setMetaOn ctx foundEnv binder@(Binder metaData xobj) =
        do let globalEnv = contextGlobalEnv ctx
               newMetaData = MetaData (Map.insert key value (getMeta metaData))
               xobjPath = getPath xobj
               prefixPath = fromMaybe [] (fmap pathToEnv foundEnv)
               fullPath = case xobjPath of
                            SymPath [] _ -> consPath prefixPath xobjPath
                            SymPath _ _ -> xobjPath
               newBinder = binder { binderMeta = newMetaData }
               newEnv = envInsertAt globalEnv fullPath newBinder
           return (ctx { contextGlobalEnv = newEnv }, dynamicNil)
primitiveMetaSet _ ctx [XObj (Sym _ _) _ _, key, _] =
  argumentErr ctx "meta-set!" "a string" "second" key
primitiveMetaSet _ ctx [target, _, _] =
  argumentErr ctx "meta-set!" "a symbol" "first" target

retroactivelyRegisterInterfaceFunctions :: Context -> String -> Ty -> IO Context
retroactivelyRegisterInterfaceFunctions ctx name t = do
  let env = contextGlobalEnv ctx
      found = multiLookupALL name env
      binders = map snd found
      resultCtx = foldl' (\maybeCtx binder -> case maybeCtx of
                                                Right ok -> registerDefnOrDefInInterfaceIfNeeded ok (binderXObj binder)
                                                Left err -> Left err)
                         (Right ctx) binders
  case resultCtx of
    Left err -> error err
    Right ctx' -> return ctx'

primitiveDefinterface :: Primitive
primitiveDefinterface xobj ctx [nameXObj@(XObj (Sym path@(SymPath [] name) _) _ _), ty] = do
  let fppl = projectFilePathPrintLength (contextProj ctx)
      typeEnv = getTypeEnv (contextTypeEnv ctx)
  case xobjToTy ty of
    Just t ->
      case lookupInEnv path typeEnv of
        Just (_, Binder _ (XObj (Lst (XObj (Interface foundType _) _ _ : _)) _ _)) ->
          -- The interface already exists, so it will be left as-is.
          if foundType == t
          then return (ctx, dynamicNil)
          else return (evalError ctx ("Tried to change the type of interface `" ++ show path ++ "` from `" ++ show foundType ++ "` to `" ++ show t ++ "`") (info xobj))
        Nothing ->
          let interface = defineInterface name t [] (info nameXObj)
              typeEnv' = TypeEnv (envInsertAt typeEnv (SymPath [] name) (Binder emptyMeta interface))
          in  do
                 newCtx <- retroactivelyRegisterInterfaceFunctions (ctx { contextTypeEnv = typeEnv' }) name t
                 return (newCtx, dynamicNil)
    Nothing ->
      return (evalError ctx ("Invalid type for interface `" ++ name ++ "`: " ++ pretty ty) (info ty))
primitiveDefinterface _ ctx [name, _] = do
  return (evalError ctx ("`definterface` expects a name as first argument, but got `" ++ pretty name ++ "`") (info name))

registerInternal :: Context -> String -> XObj -> Maybe String -> IO (Context, Either EvalError XObj)
registerInternal ctx name ty override = do
  let pathStrings = contextPath ctx
      fppl = projectFilePathPrintLength (contextProj ctx)
      globalEnv = contextGlobalEnv ctx
  case xobjToTy ty of
        Just t ->
          let path = SymPath pathStrings name
              registration = XObj (Lst [XObj (External override) Nothing Nothing,
                                        XObj (Sym path Symbol) Nothing Nothing])
                             (info ty) (Just t)
              meta = existingMeta globalEnv registration
              env' = envInsertAt globalEnv path (Binder meta registration)
          in  case registerInInterfaceIfNeeded ctx path t of
                Left errorMessage ->
                  return (makeEvalError ctx Nothing errorMessage (info ty))
                Right ctx' ->
                  do return (ctx' { contextGlobalEnv = env' }, dynamicNil)
        Nothing ->
          return (evalError ctx
            ("Can't understand type when registering '" ++ name ++ "'") (info ty))

primitiveRegister :: Primitive
primitiveRegister _ ctx [XObj (Sym (SymPath _ name) _) _ _, ty] =
  registerInternal ctx name ty Nothing
primitiveRegister _ ctx [name, _] =
  return (evalError ctx
    ("`register` expects a name as first argument, but got `" ++ pretty name ++ "`")
    (info name))
primitiveRegister _ ctx [XObj (Sym (SymPath _ name) _) _ _, ty, XObj (Str override) _ _] =
  registerInternal ctx name ty (Just override)
primitiveRegister _ ctx [XObj (Sym (SymPath _ name) _) _ _, _, override] =
  return (evalError ctx
    ("`register` expects a string as third argument, but got `" ++ pretty override ++ "`")
    (info override))
primitiveRegister _ ctx [name, _, _] =
  return (evalError ctx
    ("`register` expects a name as first argument, but got `" ++ pretty name ++ "`")
    (info name))
primitiveRegister x ctx _ =
  return (evalError ctx
    ("I didn’t understand the form `" ++ pretty x ++
     "`.\n\nIs it valid? Every `register` needs to follow the form `(register name <signature> <optional: override>)`.")
    (info x))

primitiveDeftype :: Primitive
primitiveDeftype xobj ctx (name:rest) =
  case rest of
    (XObj (Arr a) _ _ : _) -> if all isUnqualifiedSym (map fst (members a))
                             then deftype name
                             else return (makeEvalError ctx Nothing (
                                  "Type members must be unqualified symbols, but got `" ++
                                  concatMap pretty rest ++ "`") (info xobj))
                             where members (binding:val:xs) = (binding, val):members xs
                                   members [] = []
    _ -> deftype name
  where deftype name@(XObj (Sym (SymPath _ ty) _) _ _) = deftype' name ty []
        deftype (XObj (Lst (name@(XObj (Sym (SymPath _ ty) _) _ _) : tyvars)) _ _) =
          deftype' name ty tyvars
        deftype name =
          return (evalError ctx
                   ("Invalid name for type definition: " ++ pretty name)
                   (info name))
        deftype' :: XObj -> String -> [XObj] -> IO (Context, Either EvalError XObj)
        deftype' nameXObj typeName typeVariableXObjs = do
         let pathStrings = contextPath ctx
             fppl = projectFilePathPrintLength (contextProj ctx)
             env = contextGlobalEnv ctx
             typeEnv = contextTypeEnv ctx
             typeVariables = mapM xobjToTy typeVariableXObjs
             (preExistingModule, existingMeta) =
               case lookupInEnv (SymPath pathStrings typeName) env of
                 Just (_, Binder existingMeta (XObj (Mod found) _ _)) -> (Just found, existingMeta)
                 Just (_, Binder existingMeta _) -> (Nothing, existingMeta)
                 _ -> (Nothing, emptyMeta)
             (creatorFunction, typeConstructor) =
                if length rest == 1 && isArray (head rest)
                then (moduleForDeftype, Deftype)
                else (moduleForSumtype, DefSumtype)
         case (nameXObj, typeVariables) of
           (XObj (Sym (SymPath _ typeName) _) i _, Just okTypeVariables) ->
             case creatorFunction typeEnv env pathStrings typeName okTypeVariables rest i preExistingModule of
               Right (typeModuleName, typeModuleXObj, deps) ->
                 let structTy = StructTy (ConcreteNameTy typeName) okTypeVariables
                     typeDefinition =
                       -- NOTE: The type binding is needed to emit the type definition and all the member functions of the type.
                       XObj (Lst (XObj (typeConstructor structTy) Nothing Nothing :
                                  XObj (Sym (SymPath pathStrings typeName) Symbol) Nothing Nothing :
                                  rest)
                            ) i (Just TypeTy)
                     ctx' = (ctx { contextGlobalEnv = envInsertAt env (SymPath pathStrings typeModuleName) (Binder existingMeta typeModuleXObj)
                                 , contextTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) typeName typeDefinition)
                                 })
                 in do ctxWithDeps <- liftIO (foldM (define True) ctx' deps)
                       let ctxWithInterfaceRegistrations =
                             foldM (\context (path, sig) -> registerInInterfaceIfNeeded context path sig) ctxWithDeps
                                   [(SymPath (pathStrings ++ [typeModuleName]) "str", FuncTy [RefTy structTy (VarTy "q")] StringTy StaticLifetimeTy)
                                   ,(SymPath (pathStrings ++ [typeModuleName]) "copy", FuncTy [RefTy structTy (VarTy "q")] structTy StaticLifetimeTy)]
                       case ctxWithInterfaceRegistrations of
                         Left err -> do
                          liftIO (putStrLnWithColor Red err)
                          return (ctx, dynamicNil)
                         Right ok -> return (ok, dynamicNil)
               Left err ->
                 return (makeEvalError ctx (Just err) ("Invalid type definition for '" ++ pretty nameXObj ++ "':\n\n" ++ show err) Nothing)
           (_, Nothing) ->
             return (makeEvalError ctx Nothing ("Invalid type variables for type definition: " ++ pretty nameXObj) (info nameXObj))

primitiveUse :: Primitive
primitiveUse xobj ctx [XObj (Sym path _) _ _] = do
  let pathStrings = contextPath ctx
      fppl = projectFilePathPrintLength (contextProj ctx)
      env = contextGlobalEnv ctx
      e = getEnv env pathStrings
      useThese = envUseModules e
      e' = if path `elem` useThese then e else e { envUseModules = path : useThese }
  case lookupInEnv path e of
    Just (_, Binder _ _) ->
      return (ctx { contextGlobalEnv = envReplaceEnvAt env pathStrings e' }, dynamicNil)
    Nothing ->
      case lookupInEnv path env of
        Just (_, Binder _ _) ->
          return (ctx { contextGlobalEnv = envReplaceEnvAt env pathStrings e' }, dynamicNil)
        Nothing ->
          return (evalError ctx
                   ("Can't find a module named '" ++ show path ++ "'") (info xobj))

-- | Get meta data for a Binder
primitiveMeta :: Primitive
primitiveMeta (XObj _ i _) ctx [XObj (Sym path _) _ _, XObj (Str key) _ _] = do
  let pathStrings = contextPath ctx
      fppl = projectFilePathPrintLength (contextProj ctx)
      globalEnv = contextGlobalEnv ctx
  case lookupInEnv (consPath pathStrings path) globalEnv of
    Just (_, Binder metaData _) ->
        case Map.lookup key (getMeta metaData) of
          Just foundValue ->
            return (ctx, Right foundValue)
          Nothing ->
            return (ctx, dynamicNil)
    Nothing ->
      return (evalError ctx
                        ("`meta` failed, I can’t find `" ++ show path ++ "`")
                        i)
primitiveMeta _ ctx [XObj (Sym path _) _ _, key@(XObj _ i _)] =
  argumentErr ctx "meta" "a string" "second" key
primitiveMeta _ ctx [path@(XObj _ i _), _] =
  argumentErr ctx "meta" "a symbol" "first" path

primitiveDefined :: Primitive
primitiveDefined _ ctx [XObj (Sym path _) _ _] = do
  let env = contextEnv ctx
  case lookupInEnv path env of
    Just found -> return (ctx, Right trueXObj)
    Nothing -> return (ctx, Right falseXObj)
primitiveDefined _ ctx [arg] =
  argumentErr ctx "defined" "a symbol" "first" arg
