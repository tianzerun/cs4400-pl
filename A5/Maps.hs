{- |
Module      :  Maps
Description :  An implmentation of maps as associative lists.

Maintainer  :  Ferd <f.vesely@northeastern.edu>
-}

{-# LANGUAGE FlexibleInstances #-}

module Maps where

import Prelude hiding (fail)

import Control.Monad.Fail

type Map k v = [(k, v)]

-- create an empty map
empty :: Map k v
empty = []

-- add a binding to a map
add :: k -> v -> Map k v -> Map k v
add k v m = (k, v) : m

-- get a binding from a map, if it exists, otherwise return Nothing
get :: MonadFail m => Eq k => k -> Map k v -> m v
get x m = 
  case lookup x m of
       Just v -> return v
       Nothing -> fail "Couldn't find key"

-- ket all the keys in the map
keys :: Eq k => Map k v -> [k]
keys [] = []
keys ((k, _) : m) | k `notElem` keys m = k : keys m
                  | otherwise = keys m


instance MonadFail (Either String) where
  fail = Left

