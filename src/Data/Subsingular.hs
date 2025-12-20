module Data.Subsingular where

import Data.Searchable
import Data.Void

-- | Has at most one value. Has `p == q` property.
class Finite a => Subsingular a where
    subsingle :: Maybe a

instance Subsingular () where
    subsingle = Just ()

instance (Subsingular a, Subsingular b) => Subsingular (a, b) where
    subsingle = liftA2 (,) subsingle subsingle

instance Subsingular Void where
    subsingle = Nothing
