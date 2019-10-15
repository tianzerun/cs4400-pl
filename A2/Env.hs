{- |
Module      :  Env
Description :  An implementation of environments as association lists.

Maintainer  :  Zerun Tian <tian.ze@husky.neu.edu>
-}

module Env where

import SimpleTests

type Env a = [(String, a)]

-- Replace `undefined` with the appropriate Haskell expressions.

-- Construct an empty environment
empty :: Env a
empty = []

-- Add a binding of `x` to `v` to the environment `env`.
add :: String -> a -> Env a -> Env a
add x v env = (x, v) : env

-- Retrieve the binding from an environment. If the binding is not found, return
-- Nothing.
get :: String -> Env a -> Maybe a
get x [] = Nothing
get x ((y, v) : env) | x == y = Just v
                     | otherwise = get x env


-- provide your tests
tests :: IO ()
tests = do 
  test "get empty" (get "x" (empty :: Env Integer)) Nothing
  test "get add" (get "x" (add "x" 10 empty)) (Just 10)
  test "get z nothing" (get "z" (add "x" 1 (add "y" 1 empty))) Nothing
  test "get latest x" (get "x" (add "x" 10 (add "x" 1 empty))) (Just 10)
  test "get y" (get "y" (add "x" 10 (add "y" 5 empty))) (Just 5)
  test "get from list" (get "y" [("y", 0), ("y", 1)]) (Just 0)
  test "get bool value" (get "y" [("y", True), ("x", False)]) (Just True)


---------------------------- your helper functions --------------------------
