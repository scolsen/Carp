module Polymorphism 
  (nameOfPolymorphicFunction,
  ) where

import Env as E
import Obj
import Types

-- | Calculate the full, mangled name of a concretized polymorphic function.
-- For example, The 'id' in "(id 3)" will become 'id__int'.
--
-- This function uses lookupEverywhere, which gives it access to *all* possible
-- environments in the given input environment (children, (modules) parents,
-- and use modules). This allows it to derive the correct name for functions
-- that may be defined in a different environment.
--
-- TODO: Environments are passed in different order here!!!
-- TODO: We only care about a single binder here, should we use a different function?
nameOfPolymorphicFunction :: TypeEnv -> Env -> Ty -> String -> Maybe SymPath
nameOfPolymorphicFunction _ env functionType functionName =
  let foundBinder = (E.findPoly env functionName functionType)
                    <> (E.findPoly (progenitor env) functionName functionType)
                    -- <> (E.findUnifiable (allImportedEnvs env env) functionName functionType)
                    -- (E.findUnifiable [env] functionName functionType)
                    -- <> (E.findUnifiable (children env) functionName functionType)
                    -- <> (E.findUnifiable (ancestors env) functionName functionType)
   in case foundBinder of
        Right (_, (Binder _ (XObj (Lst (XObj (External (Just name)) _ _ : _)) _ _))) ->
          Just (SymPath [] name)
        Right (_, (Binder _ single)) ->
          let Just t' = xobjTy single
              (SymPath pathStrings name) = getPath single
              suffix = polymorphicSuffix t' functionType
              concretizedPath = SymPath pathStrings (name ++ suffix)
           in Just concretizedPath
        _ -> Nothing
  --let Right foundBinders = (E.findAllByName env ) <> pure (E.lookupBinderEverywhere env functionName) 
  -- in case filter ((\(Just t') -> areUnifiable functionType t') . xobjTy . binderXObj) foundBinders of
  --      [Binder _ (XObj (Lst (XObj (External (Just name)) _ _ : _)) _ _)] ->
  --        Just (SymPath [] name)
  --      [Binder _ single] ->
  --        let Just t' = xobjTy single
  --            (SymPath pathStrings name) = getPath single
  --            suffix = polymorphicSuffix t' functionType
  --            concretizedPath = SymPath pathStrings (name ++ suffix)
  --         in Just concretizedPath
  --      _ -> Nothing
