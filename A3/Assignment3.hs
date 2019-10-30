{- |
Module      :  Assignment3
Description :  Assignment 3 meta-file.

Maintainer  :  Zerun Tian <tian.ze@husky.neu.edu>
-}
module Assignment3 where

import qualified ABLF (tests)
import qualified Church (tests)

-- Fill in your name(s)
-- For a single partner: (Just "Your Name", Nothing)
-- For a pair: (Just "First Partner", Just "Second Partner")
partners :: (Maybe String, Maybe String)
partners = (Just "Zhaohao Huang", Just "Zerun Tian")

-- Please provide the total hours spent on this assignment
hoursSpent :: (Int, Int)
hoursSpent = (0, 0)

-- For Exercise 7, for each number below, fill in how many reduction steps a 
-- factorial of that number needed to reach a normal form.
factorialSteps =
  [ (1, 48)
  , (2, 196)
  , (3, 917)
  , (4, 5122)
  , (5, 33663)
  ]
---------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "== Testing module Church"
  Church.tests
  putStrLn "== Testing module ABLF"
  ABLF.tests

