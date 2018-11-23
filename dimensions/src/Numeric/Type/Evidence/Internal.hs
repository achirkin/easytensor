{-# LANGUAGE CPP             #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE ViewPatterns    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Type.Evidence.Internal
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Extract a Constraint from an Evidence to manipulate it as a plain value.
-- It is supposed to be used in typechecker plugins only
--   -- to move around instances of type classes.  
--
-----------------------------------------------------------------------------
module Numeric.Type.Evidence.Internal
  ( BareConstraint, pattern EvValue
  ) where


import           GHC.Base              (Constraint, Type, unsafeCoerce#)
import           Numeric.Type.Evidence

-- | An unsafeCoerced pointer to a Constraint, such as a class function dictionary.
data BareConstraint :: Constraint -> Type

-- | Extract the constraint inside the Evidence GADT as if it was
--   an ordinary value of kind `Type`.
--
--   It exploits the feature of the GHC core language
--    -- representing constraints as ordinary type arguments of function.
--   Thus, I unsafeCoerce between a function with one argument and a function
--    with no arguments and one constraint.
--
--   This pattern has never been tested with multiple constraints.
pattern EvValue :: BareConstraint c -> Evidence c
pattern EvValue c <- (evToVal -> c)
  where
    EvValue c = valToEv c

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE EvValue #-}
#endif

evToVal :: Evidence c -> BareConstraint c
evToVal E = case unsafeCoerce# id of MagicBC c -> c
{-# INLINE evToVal #-}

valToEv :: BareConstraint c -> Evidence c
valToEv = unsafeCoerce# (MagicEv E)
{-# INLINE valToEv #-}

newtype MagicEv c = MagicEv (c => Evidence c)
newtype MagicBC c = MagicBC (c => BareConstraint c)
