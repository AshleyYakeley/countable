module Data.Singular where

import Data.Countable
import Data.Subsingular

-- | There is exactly one value.
class (Subsingular a, AtLeastOneCountable a) => Singular a where
    single :: a

instance Singular () where
    single = ()

instance (Singular a, Singular b) => Singular (a, b) where
    single = (single, single)
