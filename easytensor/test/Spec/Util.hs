{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Util (propWithTypes, testWithTypes) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Language.Haskell.TH
import Test.QuickCheck
-- import Language.Haskell.TH.Syntax

-- | Test a property with several types.
--
--   The first argument is the name of a @Testable@ function
--
--   The second argument is expected to be a type synonym of kind [k] --
--     the variants of type arguments to apply to a fun
--
--   Kind @k@ here should match the kind of type variable of the testable function,
--    (or a promoted n-tuple or a promoted list of such variables.)
testWithTypes :: Name -- ^ Function with a few type variables
              -> Name -- ^ Name of a type with kind [k]
              -> ExpQ
testWithTypes fName tyNames = do
    typeLists <- getTypes tyNames
    let props = (\ts -> mkProp ts $ foldl AppTypeE (VarE fName) ts) <$> typeLists
    [e| conjoin $(listE props) |]
  where
    mkProp :: [Type] -> Exp -> ExpQ
    mkProp ts ex
     = let qe = pure ex :: ExpQ
           qs = litE . stringL $ unlines ["Tested with types:", pprint ts]
       in [e| property ( counterexample $qs $qe ) |]

-- | Assuming the argument is a type synonym for a list of types, unpack it.
getTypes :: Name -> Q [[Type]]
getTypes tyNames =
    reify tyNames >>= unpackTyConI >>= unpackFamList >>= traverse unpackAppType
  where
    unpackTyConI :: Info -> Q Type
    unpackTyConI (TyConI (TySynD _ [] t)) = pure t
    unpackTyConI i = error $
      "getTypes/unpackTyConI:"
      ++ "Expected a type synonym with no arguments, but got something else "
      ++ show i
    unpackFamList :: Type -> Q [Type]
    unpackFamList t
      | Just ts <- unpackPList t = pure ts
      | otherwise                = error $
      "getTypes/unpackFamList:"
      ++ "Expected a promoted (type) list inside the type synonym, but got "
      ++ show t
    unpackAppType :: Type -> Q [Type]
    unpackAppType t
      | Just ts <- unpackPList t  = pure ts
      | Just ts <- unpackPTuple t = pure ts
      | otherwise                 = pure [t]
    unpackPTuple :: Type -> Maybe [Type]
    unpackPTuple (PromotedTupleT _) = pure []
    unpackPTuple (AppT t r)         = (++ [r]) <$> unpackPTuple t
    unpackPTuple (ParensT t)        = unpackPTuple t
    unpackPTuple (SigT t _)         = unpackPTuple t
    unpackPTuple _                  = Nothing
    unpackPList :: Type -> Maybe [Type]
    unpackPList PromotedNilT                    = pure []
    unpackPList (AppT (AppT PromotedConsT r) t) = (r:) <$> unpackPList t
    unpackPList (ParensT t)                     = unpackPList t
    unpackPList (SigT t _)                      = unpackPList t
    unpackPList _                               = Nothing


-- | Make several properties by applying types
propWithTypes :: Name -- ^ Function with a few type variables
              -> [[Name]] -- ^ All specific types to test it on.
              -> DecsQ
propWithTypes fName tyNames
    = fmap join . sequence
    $ zipWith3 mkDecl funNames funTypes appliedExpressions
  where
    appliedExpressions :: [ExpQ]
    appliedExpressions
      = foldl (\e -> appTypeE e . conT) (varE fName) <$> tyNames
    funNames :: [Q Name]
    funNames
      = newName . foldl (\n -> (n ++) . nameBase) ("prop_" ++ nameBase fName) <$> tyNames
    funTypes :: [TypeQ]
    funTypes
      = (\ns -> fmap (subtTyVars (map ConT ns)) ) <$> tyNames <*> [infoType <$> reify fName]
    mkDecl qn qt qe = do
      n <- qn
      sequence
        [ sigD n qt
        , funD n [(\e -> Clause [] (NormalB e) []) <$> qe]
        ]


-- | Get a type of something at the term level (e.g. function, var, constructor).
infoType :: Info -> Type
infoType (ClassOpI _ t _) = t
infoType (DataConI _ t _) = t
infoType (VarI _ t _)     = t
infoType _                = undefined

-- | Substitute at most as many type variables as given types in the list.
--
--   For every type in a list @ts@, the function looks for the first occurrence
--   of any type variable and substitutes all its occurrences in the target type.
--
--   Returns the original type if it has no type variables.
subtTyVars :: [Type] -> Type -> Type
subtTyVars = flip (foldl f)
  where
    f t x
      | Just v <- findFirstTyVar t
        = substTyVar v x t
      | otherwise = t

-- | Substitute all occurrences of a type variable
substTyVar :: Name -- ^ TyVar name
           -> Type -- ^ Substitution
           -> Type -- ^ Original type
           -> Type
substTyVar x y (ForallT vs ctx t)
    = ForallT (remVar vs) (ctx >>= substOrRemove) (substTyVar x y t)
  where
    thisVar :: TyVarBndr -> Bool
    thisVar (PlainTV n)    = n == x
    thisVar (KindedTV n _) = n == x
    remVar :: [TyVarBndr] -> [TyVarBndr]
    remVar = filter (not . thisVar)
    substOrRemove p
      = let p' = substTyVar x y p
        in if findFirstTyVar p' == Nothing then [] else [p]
substTyVar x y (AppT t1 t2) = AppT (substTyVar x y t1) (substTyVar x y t2)
substTyVar x y (SigT t1 t2) = SigT (substTyVar x y t1) (substTyVar x y t2)
substTyVar x y v@(VarT n) = if n == x then y else v
substTyVar x y (InfixT t1 n t2) = InfixT (substTyVar x y t1) n (substTyVar x y t2)
substTyVar x y (UInfixT t1 n t2) = UInfixT (substTyVar x y t1) n (substTyVar x y t2)
substTyVar x y (ParensT t) = ParensT (substTyVar x y t)
substTyVar _ _ t = t

findFirstTyVar :: Type -> Maybe Name
findFirstTyVar (ForallT (PlainTV n:_) _ _) = Just n
findFirstTyVar (ForallT (KindedTV n _:_) _ _) = Just n
findFirstTyVar (ForallT [] ctx t)
  = getFirst (foldMap (First . findFirstTyVar) ctx) <|> findFirstTyVar t
findFirstTyVar (AppT t1 t2) = findFirstTyVar t1 <|> findFirstTyVar t2
findFirstTyVar (SigT t1 k1) = findFirstTyVar t1 <|> findFirstTyVar k1
findFirstTyVar (VarT n) = Just n
findFirstTyVar (InfixT t1 _ t2) = findFirstTyVar t1 <|> findFirstTyVar t2
findFirstTyVar (UInfixT t1 _ t2) = findFirstTyVar t1 <|> findFirstTyVar t2
findFirstTyVar (ParensT t) = findFirstTyVar t
findFirstTyVar _ = Nothing
