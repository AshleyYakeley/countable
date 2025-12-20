{-# OPTIONS -fno-warn-orphans #-}

module Data.Function.Show
    (
    )
where

import Data.List
import Prelude

import Data.Searchable

instance (Show a, Finite a, Show b) => Show (a -> b) where
    show f = "{" ++ (intercalate "," (fmap (\a -> (show a) ++ "->" ++ (show (f a))) allValues)) ++ "}"
