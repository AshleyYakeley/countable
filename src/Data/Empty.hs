{-# OPTIONS -fno-warn-orphans #-}
module Data.Empty where

import Data.Void
import Data.Subsingular

-- | There are no values.
class Subsingular n => Empty n where
    never :: n -> a

instance Empty Void where
    never = absurd

instance (Empty a, Subsingular b) => Subsingular (Either a b) where
    subsingle = fmap Right subsingle

instance (Empty a, Empty b) => Empty (Either a b) where
    never (Left a) = never a
    never (Right a) = never a
