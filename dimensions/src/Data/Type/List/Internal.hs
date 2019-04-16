{-# OPTIONS_GHC -fobject-code       #-}
{-# OPTIONS_GHC -fplugin Data.Type.List.InjectiveSnoc #-}
{-# OPTIONS_HADDOCK hide, prune     #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Type.List.Internal
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Thanks to @Data.Type.List.InjectiveSnoc@, @Snoc@ appears to be injective
-- for an oustide viewer.
--
--
--------------------------------------------------------------------------------
module Data.Type.List.Internal ( Snoc ) where

-- | Appending a list on the other side.
type family Snoc (xs :: [k]) (x :: k) = (ys :: [k]) where
    Snoc '[]       y = '[y]
    Snoc (x ': xs) y = x ': Snoc xs y
