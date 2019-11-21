{- |
Module      :  Store
Description :  An implementation of stores using Haskell's Map datatype.

Maintainer  :  Ferd <f.vesely@northeastern.edu>
-}

module Store where

import Data.Map.Strict as Map

type Store = Map.Map String

-- empty store
empty :: Store a
empty = Map.empty

-- update a variable with a value
add :: String -> a -> Store a -> Store a
add = Map.insert

-- get the current value associated with a variable
get :: String -> Store a -> Maybe a
get = Map.lookup

-- convert a list to a store
fromList :: [(String, a)] -> Store a
fromList = Map.fromList



