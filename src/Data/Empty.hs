module Data.Empty where

import Data.Countable
import Data.Searchable

class (Finite n) => Empty n where
    never :: n -> a

instance (Empty a, Empty b) => Empty (Either a b) where
    never (Left a) = never a
    never (Right a) = never a

instance (Empty a, Finite b) => Empty (a, b) where
    never (a, _) = never a

instance (AtLeastOneCountable a, Finite a, Empty b) => Empty (a -> b) where
    never ab = never (ab countFirst)

data None

instance Countable None where
    countPrevious = never
    countMaybeNext Nothing = Nothing
    countMaybeNext (Just n) = never n

instance Searchable None where
    search = finiteSearch

instance Finite None where
    allValues = []
    assemble _ = pure never

instance Empty None where
    never a = case a of {}

instance Eq None where
    a == _b = never a

instance Ord None where
    a <= _b = never a

instance Show None where
    show a = never a
