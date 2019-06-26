{-# OPTIONS_GHC -fplugin Data.Type.List.InjectiveSnoc #-}
{-# OPTIONS_HADDOCK hide, prune #-}
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
module Data.Type.List.Internal () where


-- -- | Appending a list on the other side.
-- type family Snoc (xs :: [k]) (x :: k) = (ys :: [k]) where
--     Snoc (x ': xs) y = x ': Snoc xs y
--     Snoc '[]       y = '[y]
--
-- -- | Extract the last element of a list, which must be non-empty.
-- type family Last (xs :: [k]) :: k where
--     Last ('[] :: [k]) = TypeError ( ListError k "Last: empty type-level list." )
--     Last '[x]         = x
--     Last (_ ': xs)    = Last xs
--
-- -- | Extract all but last elements of a list, which must be non-empty.
-- type family Init (xs :: [k]) :: [k] where
--     Init ('[] :: [k]) = TypeError ( ListError k "Init: empty type-level list." )
--     Init '[x]         = '[]
--     Init (x ': xs)    = x ': Init xs
