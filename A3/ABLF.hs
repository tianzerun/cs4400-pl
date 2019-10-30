{- |
Module      :  ABLF
Description :  Syntax of the ABLF language and a translation function to pure 
               lambda calculus.

Maintainer  :  Zerun Tian <tian.ze@husky.neu.edu>
-}


module ABLF where

import Lambda
import Church

import Reduce   -- for testing

import SimpleTests


data ABLFExpr = AVar Variable
              | Num Integer             -- operations on naturals
              | Add ABLFExpr ABLFExpr
              | Sub ABLFExpr ABLFExpr
              | Mul ABLFExpr ABLFExpr
              | Exp ABLFExpr ABLFExpr

              | Bool Bool               -- operations on booleans
              | And ABLFExpr ABLFExpr
              | Or ABLFExpr ABLFExpr
              | Not ABLFExpr

              | Leq ABLFExpr ABLFExpr   -- Leq n m: "Is n less or equal to m?"
              | Eq ABLFExpr ABLFExpr    -- Eq n m: "Is n equal to m?"
     
              | IfThen ABLFExpr ABLFExpr ABLFExpr  -- conditional expression

              | Let Variable ABLFExpr ABLFExpr     -- let x = e1 in e2

              -- recursive function definitions
              | LetFun Variable [Variable] ABLFExpr ABLFExpr
              -- function application
              | Call Variable [ABLFExpr]
              deriving (Eq, Show)

translate :: ABLFExpr -> Lambda
translate (AVar v) = Var v
translate (Num i) = toNumeral i
translate (Add e1 e2) = App (App cplus (translate e1)) (translate e2)
translate (Sub e1 e2) = App (App cminus (translate e1)) (translate e2)
translate (Mul e1 e2) = App (App ctimes (translate e1)) (translate e2)
translate (Exp e1 e2) = App (App expn (translate e1)) (translate e2)
translate (Bool b) = toChurchBool b
translate (And e1 e2) = App (App cand (translate e1)) (translate e2)
translate (Or e1 e2) = App (App cor (translate e1)) (translate e2)
translate (Not e) = App cnot (translate e)
translate (Leq e1 e2) = App (App cleq (translate e1)) (translate e2)
translate (Eq e1 e2) = App (App ceq (translate e1)) (translate e2)
translate (IfThen e1 e2 e3) = App (App (App cifthen (translate e1)) (translate e2)) (translate e3)
translate (Let x e1 e2) = App (Lam x (translate e2)) (translate e1)
translate (LetFun f vars e1 e2) = App (Lam f (translate e2)) (App fix (Lam f (curryVars vars e1)))
translate (Call f args) = callHelper f args


factorialOf :: Integer -> ABLFExpr
factorialOf n = (LetFun "f"
                        ["n"]
                        (IfThen (Eq (AVar "n") (Num 1))
                                (Num 1)
                                (Mul (AVar "n") (Call "f" [(Sub (AVar "n") (Num 1))])))
                        (Call "f" [(Num n)]))

---- tests

tests :: IO ()
tests = do
  test "translate (AVar x)"
       (translate (AVar "x"))
       (Var "x")
  test "translate (Num 10)"
       (fromNumeral (translate (Num 10)))
       (Just 10)
  test "translate (Add (Num 1) (Num 2))"
       (fromNumeral (normalize (translate (Add (Num 1) (Num 2)))))
       (Just (1 + 2))
  test "translate (Sub (Num 2) (Num 1)"
       (fromNumeral (normalize (translate (Sub (Num 2) (Num 1)))))
       (Just (2 - 1))
  test "translate (Sub (Num 1) (Num 2)"
       (fromNumeral (normalize (translate (Sub (Num 1) (Num 2)))))
       (Just 0)
  test "translate (Sub (Num 2) (Num 2)"
       (fromNumeral (normalize (translate (Sub (Num 2) (Num 2)))))
       (Just (2 - 2))
  test "translate (Mul (Num 3) (Num 2)"
       (fromNumeral (normalize (translate (Mul (Num 3) (Num 2)))))
       (Just (3 * 2))
  test "translate (Mul (Num 0) (Num 2)"
       (fromNumeral (normalize (translate (Mul (Num 0) (Num 2)))))
       (Just (0 * 2))
  test "translate (Mul (Num 2) (Num 0)"
       (fromNumeral (normalize (translate (Mul (Num 2) (Num 0)))))
       (Just (2 * 0))
  test "translate (Bool True)"
       (fromChurchBool (translate (Bool True)))
       (Just True)
  test "translate (Bool False)"
       (fromChurchBool (translate (Bool False)))
       (Just False)
  test "translate (And True True)"
       (fromChurchBool (normalize (translate (And (Bool True) (Bool True)))))
       (Just (True && True))
  test "translate (And False True)"
       (fromChurchBool (normalize (translate (And (Bool False) (Bool True)))))
       (Just (False && True))
  test "translate (And True False)"
       (fromChurchBool (normalize (translate (And (Bool True) (Bool False)))))
       (Just (True && False))
  test "translate (And False False)"
       (fromChurchBool (normalize (translate (And (Bool False) (Bool False)))))
       (Just (False && False))
  test "translate (Or True True)"
       (fromChurchBool (normalize (translate (Or (Bool True) (Bool True)))))
       (Just (True || True))
  test "translate (Or False True)"
       (fromChurchBool (normalize (translate (Or (Bool False) (Bool True)))))
       (Just (False || True))
  test "translate (Or True False)"
       (fromChurchBool (normalize (translate (Or (Bool True) (Bool False)))))
       (Just (True || False))
  test "translate (Or False False)"
       (fromChurchBool (normalize (translate (Or (Bool False) (Bool False)))))
       (Just (False || False))
  test "translate (Not True)"
       (fromChurchBool (normalize (translate (Not (Bool True)))))
       (Just (not True))
  test "translate (Not False)"
       (fromChurchBool (normalize (translate (Not (Bool False)))))
       (Just (not False))
  test "translate (Not (Or False False))"
       (fromChurchBool (normalize (translate (Not (Or (Bool False) (Bool False))))))
       (Just (not (False || False)))
  test "translate (Leq (Num 1) (Num 2))"
       (fromChurchBool (normalize (translate (Leq (Num 1) (Num 2)))))
       (Just (1 <= 2))
  test "translate (Leq (Num 2) (Num 1))"
       (fromChurchBool (normalize (translate (Leq (Num 2) (Num 1)))))
       (Just (2 <= 1))
  test "translate (Leq (Num 2) (Num 2))"
       (fromChurchBool (normalize (translate (Leq (Num 2) (Num 2)))))
       (Just (2 <= 2))
  test "translate (Eq (Num 1) (Num 1))"
       (fromChurchBool (normalize (translate (Eq (Num 1) (Num 1)))))
       (Just (1 == 1))
  test "translate (Eq (Num 3) (Num 1))"
       (fromChurchBool (normalize (translate (Eq (Num 3) (Num 1)))))
       (Just (3 == 1))
  test "translate (IfThen True (Num 1) (Num 2))"
       (fromNumeral (normalize (translate (IfThen (Bool True) (Num 1) (Num 2)))))
       (Just (if True then 1 else 2))
  test "translate (IfThen False (Num 1) (Num 2))"
       (fromNumeral (normalize (translate (IfThen (Bool False) (Num 1) (Num 2)))))
       (Just (if False then 1 else 2))
  test "translate (IfThen (Leq 1 2) True False)"
       (fromChurchBool (normalize (translate (IfThen (Leq (Num 1) (Num 2)) (Bool True) (Bool False)))))
       (Just (if (1 <= 2) then True else False))
  test "translate (IfThen (Leq 2 1) True False)"
       (fromChurchBool (normalize (translate (IfThen (Leq (Num 2) (Num 1)) (Bool True) (Bool False)))))
       (Just (if (2 <= 1) then True else False))
  test "translate (Let x 1 (+ x 1))"
       (fromNumeral (normalize (translate (Let "x" (Num 1) (Add (AVar "x") (Num 1))))))
       (Just (let x = 1 in x + 1))
  test "translate (Let x True (And True False))"
       (fromChurchBool (normalize (translate (Let "x" (Bool True) (And (AVar "x") (Bool False))))))
       (Just (let x = True in (x && False)))
  test "translate (LetFun f [a] (Add a 2) (f 1)"
       (fromNumeral (normalize (translate (LetFun "f" ["a"] (Add (AVar "a") (Num 2))
                                                  (Call "f" [(Num 1)])))))
       (Just (let f a = (a + 2) in f 1))
  test "translate (LetFun f [a, b] (Add a b) (f 1 2)"
       (fromNumeral (normalize (translate (LetFun "f" ["a", "b"] (Add (AVar "a") (AVar "b"))
                                                  (Call "f" [(Num 1), (Num 2)])))))
       (Just (let f a b = a + b in (f 1 2)))
  test "translate (LetFun f [a] (if (a == 0) then a else (a + (f (a - 1)))) (f 2)"
       (fromNumeral (normalize (translate (LetFun "f"
                                                  ["a"]
                                                  (IfThen (Eq (AVar "a") (Num 0))
                                                          (AVar "a")
                                                          (Add (AVar "a") (Call "f" [(Sub (AVar "a") (Num 1))])))
                                                  (Call "f" [(Num 2)])))))
       (Just (let f a = if a == 0 then a else a + (f (a - 1)) in (f 2)))
  test "translate (Exp (Num 2) (Num 0))"
       (fromNumeral (normalize (translate (Exp (Num 2) (Num 0)))))
       (Just (2 ^ 0))
  test "translate (Exp (Num 0) (Num 2))"
       (fromNumeral (normalize (translate (Exp (Num 0) (Num 2)))))
       (Just (0 ^ 2))
  test "translate (Exp (Num 2) (Num 5))"
       (fromNumeral (normalize (translate (Exp (Num 2) (Num 5)))))
       (Just (2 ^ 5))


---------------------------- your helper functions --------------------------
curryVars :: [Variable] -> ABLFExpr -> Lambda
curryVars [] e = translate e
curryVars (var : vars) e = (Lam var (curryVars vars e))

callHelper :: Variable -> [ABLFExpr] -> Lambda
callHelper f [] = (Var f)
callHelper f (arg : args) = (App (callHelper f args) (translate arg))