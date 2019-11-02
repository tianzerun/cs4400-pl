{- |
Module      :  Church
Description :  Church encodings of booleans and natural numbers. 
               Fixpoint operator.

Maintainer  :  Zerun Tian <tian.ze@husky.neu.edu>
-}


module Church where

import Lambda
import Reduce

import SimpleTests

toChurchBool :: Bool -> Lambda
toChurchBool True = Lam "t" (Lam "f" (Var "t"))
toChurchBool False = Lam "t" (Lam "f" (Var "f"))

fromChurchBool :: Lambda -> Maybe Bool
fromChurchBool (Lam a (Lam b (Var c))) =
  if c == a
    then Just True
    else if c == b
          then Just False
          else Nothing
fromChurchBool _ = Nothing

toNumeral :: Integer -> Lambda
toNumeral i = Lam "s" (Lam "z" (toNumeralHelper i))

fromNumeral :: Lambda -> Maybe Integer
fromNumeral (Lam s (Lam z lam)) = fromNumeralHelper lam s z
fromNumeral _ = Nothing

csucc :: Lambda
csucc = Lam "n" (Lam "s" (Lam "z" 
        (App (Var "s") (App (App (Var "n") (Var "s")) (Var "z")))))

cpred :: Lambda
cpred = Lam "n" (Lam "f" (Lam "x" (
          App (
            App (
              App (Var "n") 
                  (Lam "g" (Lam "h" (
                    App (Var "h") (App (Var "g") (Var "f")))))) 
              (Lam "u" (Var "x"))) 
            (Lam "u" (Var "u")))))

-- operations on numerals
cplus :: Lambda    -- addition
cplus = Lam "m" (Lam "n" (App (App (Var "m") csucc) (Var "n")))

cminus :: Lambda   -- subtraction
cminus = Lam "m" (Lam "n" (App (App (Var "n") cpred) (Var "m")))

ctimes :: Lambda   -- multiplication
ctimes = Lam "m" (Lam "n" (App (App (Var "m") (App cplus (Var "n"))) (toNumeral 0)))

-- operations on Church booleans
cand :: Lambda
cand = Lam "a" (Lam "b" (App (App (Var "a") (Var "b")) (toChurchBool False)))

cor :: Lambda 
cor = Lam "a" (Lam "b" (App (App (Var "a") (toChurchBool True)) (Var "b")))

cnot :: Lambda
cnot = Lam "a" (App (App (Var "a") (toChurchBool False)) (toChurchBool True))

-- operations on numerals returning Church booleans
ciszero :: Lambda
ciszero = Lam "n" (App (App (Var "n") (Lam "x" (toChurchBool False))) (toChurchBool True))

cleq :: Lambda     -- less or equal
cleq = Lam "m" (Lam "n" (App ciszero (App (App cminus (Var "m")) (Var "n"))))

ceq :: Lambda      -- equal
ceq = Lam "m" (Lam "n" (App (App cand (App (App cleq (Var "m")) (Var "n"))) (App (App cleq (Var "n")) (Var "m"))))

-- conditional expression
cifthen :: Lambda
cifthen = Lam "b" (Lam "t" (Lam "f" (App (App (Var "b") (Var "t")) (Var "f"))))

-- fixpoint combinator
fix :: Lambda
fix = Lam "f" (App (Lam "x" (App (Var "f") (App (Var "x") (Var "x"))))
                   (Lam "x" (App (Var "f") (App (Var "x") (Var "x")))))

-- exponentiation n^m <=> lam n m . m (times n) one
expn :: Lambda
expn = Lam "n" (Lam "m" (App (App (Var "m") (App ctimes (Var "n"))) (toNumeral 1)))

------ tests go here

tests :: IO ()
tests = do
  test "toChurchBool True -> fromChurchBool"
       (fromChurchBool (toChurchBool True))
       (Just True)
  test "toChurchBool False -> fromChurchBool"
       (fromChurchBool (toChurchBool False))
       (Just False)
  test "toNumeral 0 -> fromNumeral"
       (fromNumeral (toNumeral 0))
       (Just 0)
  test "toNumeral 1 -> fromNumeral"
       (fromNumeral (toNumeral 1))
       (Just 1)
  test "toNumeral 2 -> fromNumeral"
       (fromNumeral (toNumeral 2))
       (Just 2)
  test "toNumeral 10 -> fromNumeral"
       (fromNumeral (toNumeral 10))
       (Just 10)
  test "plus 0 0 --> 0"
       (fromNumeral (normalize (App (App cplus (toNumeral 0)) (toNumeral 0))))
       (Just (0 + 0))
  test "plus 3 5 --> 8"
       (fromNumeral (normalize (App (App cplus (toNumeral 3)) (toNumeral 5))))
       (Just (3 + 5))
  test "minus 6 6 --> 0"
       (fromNumeral (normalize (App (App cminus (toNumeral 5)) (toNumeral 6))))
       (Just (6 - 6))
  test "minus 5 3 --> 2"
       (fromNumeral (normalize (App (App cminus (toNumeral 5)) (toNumeral 3))))
       (Just (5 - 3))
  test "minus 3 4 --> 0"
       (fromNumeral (normalize (App (App cminus (toNumeral 3)) (toNumeral 4))))
       (Just 0)
  test "times 2 0 --> 0"
       (fromNumeral (normalize (App (App ctimes (toNumeral 2)) (toNumeral 0))))
       (Just (2 * 0))
  test "times 2 3 --> 6"
       (fromNumeral (normalize (App (App ctimes (toNumeral 2)) (toNumeral 3))))
       (Just (2 * 3))
  test "and True True --> True"
       (fromChurchBool (normalize (App (App cand (toChurchBool True)) (toChurchBool True))))
       (Just (True && True))
  test "and False True --> False"
       (fromChurchBool (normalize (App (App cand (toChurchBool False)) (toChurchBool True))))
       (Just (False && True))
  test "and True False --> False"
       (fromChurchBool (normalize (App (App cand (toChurchBool True)) (toChurchBool False))))
       (Just (True && False))
  test "and False False --> False"
       (fromChurchBool (normalize (App (App cand (toChurchBool False)) (toChurchBool False))))
       (Just (False && False))
  test "or True True --> True"
       (fromChurchBool (normalize (App (App cor (toChurchBool True)) (toChurchBool True))))
       (Just (True || True))
  test "or False True --> True"
       (fromChurchBool (normalize (App (App cor (toChurchBool False)) (toChurchBool True))))
       (Just (False || True))
  test "or True False --> True"
       (fromChurchBool (normalize (App (App cor (toChurchBool True)) (toChurchBool False))))
       (Just (True || False))
  test "or False False --> False"
       (fromChurchBool (normalize (App (App cor (toChurchBool False)) (toChurchBool False))))
       (Just (False || False))
  test "not True --> False"
       (fromChurchBool (normalize (App cnot (toChurchBool True))))
       (Just (not True))
  test "not False --> True"
       (fromChurchBool (normalize (App cnot (toChurchBool False))))
       (Just (not False))
  test "is-zero 0 --> True"
       (fromChurchBool (normalize (App ciszero (toNumeral 0))))
       (Just (0 == 0))
  test "is-zero 5 --> False"
       (fromChurchBool (normalize (App ciszero (toNumeral 5))))
       (Just (5 == 0))
  test "leq 0 0 --> True"
       (fromChurchBool (normalize (App (App cleq (toNumeral 0)) (toNumeral 0))))
       (Just (0 <= 0))
  test "leq 0 1 --> True"
       (fromChurchBool (normalize (App (App cleq (toNumeral 0)) (toNumeral 1))))
       (Just (0 <= 1))
  test "leq 1 0 --> False"
       (fromChurchBool (normalize (App (App cleq (toNumeral 1)) (toNumeral 0))))
       (Just (1 <= 0))
  test "leq 5 10 --> True"
       (fromChurchBool (normalize (App (App cleq (toNumeral 5)) (toNumeral 10))))
       (Just (5 <= 10))
  test "leq 10 5 --> False"
       (fromChurchBool (normalize (App (App cleq (toNumeral 10)) (toNumeral 5))))
       (Just (10 <= 5))
  test "equal 0 0 --> True"
       (fromChurchBool (normalize (App (App ceq (toNumeral 0)) (toNumeral 0))))
       (Just (0 == 0))
  test "equal 0 1 --> False"
       (fromChurchBool (normalize (App (App ceq (toNumeral 0)) (toNumeral 1))))
       (Just (0 == 1))
  test "equal 3 2 --> False"
       (fromChurchBool (normalize (App (App ceq (toNumeral 3)) (toNumeral 2))))
       (Just (3 == 2))
  test "if-then True 2 3 -> 2"
       (fromNumeral (normalize (App (App (App cifthen (toChurchBool True)) (toNumeral 2)) (toNumeral 3))))
       (Just 2)
  test "if-then False 2 3 -> 3"
       (fromNumeral (normalize (App (App (App cifthen (toChurchBool False)) (toNumeral 2)) (toNumeral 3))))
       (Just 3)
  test "expn 1 2 -> 1"
       (fromNumeral (normalize (App (App expn (toNumeral 1)) (toNumeral 2))))
       (Just (1 ^ 2))
  test "expn 2 3 -> 8"
       (fromNumeral (normalize (App (App expn (toNumeral 2)) (toNumeral 3))))
       (Just (2 ^ 3))
  test "expn 0 5 -> 1"
       (fromNumeral (normalize (App (App expn (toNumeral 0)) (toNumeral 5))))
       (Just (0 ^ 5))
  test "expn 3 0 -> 1"
       (fromNumeral (normalize (App (App expn (toNumeral 3)) (toNumeral 0))))
       (Just (3 ^ 0))
  test "expn 3 3 -> 27"
       (fromNumeral (normalize (App (App expn (toNumeral 3)) (toNumeral 3))))
       (Just (3 ^ 3))

---------------------------- your helper functions --------------------------
toNumeralHelper :: Integer -> Lambda
toNumeralHelper 0 = (Var "z")
toNumeralHelper i = (App (Var "s") (toNumeralHelper (i - 1)))

fromNumeralHelper :: Lambda -> String -> String -> Maybe Integer
fromNumeralHelper (Var v) s z =
  if v == z
    then Just 0
    else Nothing
fromNumeralHelper (App (Var v) app) s z =
  case fromNumeralHelper app s z of
    Just i -> if v == s then Just (1 + i) else Nothing
    Nothing -> Nothing
fromNumeralHelper _ _ _ = Nothing