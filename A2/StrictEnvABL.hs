{- |
Module      :  StrictEnvABL
Description :  A strict, environment-based implementation of ABL.

Maintainer  :  Zerun Tian <tian.ze@husky.neu.edu>
-}

module StrictEnvABL where

import Env
import ABL

import SimpleTests

import Debug.Trace (trace) -- for debugging purposes


-- Check application of binary operations to `ABLValue`s
type BinOp a = (a -> a -> a)

-- Apply an binary integer operation
applyIntegerBinOp :: BinOp Integer -> ABLValue -> ABLValue -> Maybe ABLValue
applyIntegerBinOp f (Num n1) (Num n2) = Just (Num (f n1 n2))
applyIntegerBinOp f _ _ = Nothing

-- Apply a binary boolean operation
applyBoolBinOp :: BinOp Bool -> ABLValue -> ABLValue -> Maybe ABLValue
applyBoolBinOp f (Bool b1) (Bool b2) = Just (Bool (f b1 b2))
applyBoolBinOp f _ _ = Nothing

-- Evaluate an ABL expression in the given environment
evalABL :: Env ABLValue -> ABLExpr -> Maybe ABLValue
evalABL env (Var x) = get x env
evalABL env (Val v) = Just v
evalABL env (Add e1 e2) = 
  case evalABL env e1 of
       Just v1 -> case evalABL env e2 of
                       Just v2 -> applyIntegerBinOp (+) v1 v2
                       Nothing -> Nothing
       Nothing -> Nothing
evalABL env (Sub e1 e2) =
  case evalABL env e1 of
       Just v1 -> case evalABL env e2 of
                       Just v2 -> applyIntegerBinOp (-) v1 v2
                       Nothing -> Nothing
       Nothing -> Nothing
evalABL env (Mul e1 e2) =
  case evalABL env e1 of
       Just v1 -> case evalABL env e2 of
                       Just v2 -> applyIntegerBinOp (*) v1 v2
                       Nothing -> Nothing
       Nothing -> Nothing
evalABL env (Div e1 e2) =
  case evalABL env e1 of
       Just v1 -> case evalABL env e2 of
                       Just v2 -> case v2 of
                                       Num 0 -> Nothing
                                       v2 -> applyIntegerBinOp (div) v1 v2
                       Nothing -> Nothing
       Nothing -> Nothing
evalABL env (Eq e1 e2) =
  case evalABL env e1 of
       Just v1 -> case evalABL env e2 of
                       Just v2 -> case v1 of
                                       Num num1 -> case v2 of
                                                        Num num2 -> Just (Bool (num1 == num2))
                                                        _ -> Nothing
                                       Bool b1 -> case v2 of
                                                       Bool b2 -> Just (Bool (b1 == b2))
                                                       _ -> Nothing
                       Nothing -> Nothing
       Nothing -> Nothing
evalABL env (And e1 e2) =
  case evalABL env e1 of
       Just v1 -> case evalABL env e2 of
                       Just v2 -> applyBoolBinOp (&&) v1 v2
                       Nothing -> Nothing
       Nothing -> Nothing
evalABL env (Or e1 e2) =
  case evalABL env e1 of
       Just v1 -> case evalABL env e2 of
                       Just v2 -> applyBoolBinOp (||) v1 v2
                       Nothing -> Nothing
       Nothing -> Nothing
evalABL env (Not e) =
  case evalABL env e of
       Just v -> case v of
                      Bool b -> Just (Bool (not b))
                      _ -> Nothing
       Nothing -> Nothing
evalABL env (Let1 x e1 e2) =
  case evalABL env e1 of
       Just v1 -> let e' = add x v1 env in evalABL e' e2
       Nothing -> Nothing
evalABL env (If e1 e2 e3) =
  case evalABL env e1 of
       Just (Bool b) -> if b then (evalABL env e2) else (evalABL env e3)
       _ -> Nothing
evalABL env (Fresh e) = evalABL empty e
evalABL env (LetStar vars e) = evalABL env (unfoldLetStar vars e)


-- Check if the ABL expression is well-scoped, that is if all variables are 
-- defined before they are used.
--
-- The function is defined using a helper function scopeCheckAux. Your task is 
-- to complete the definition of scopeCheckAux.
scopeCheck :: ABLExpr -> Bool
scopeCheck e = scopeCheckAux [] e


-- Check if the given expression is well-scoped, provided the given variables
-- are visible. That is, scopeCheckAux vars (Var x) is well-scoped only if x 
-- appears in vars. For example, scopeCheck ["x"] (Var "x") is True, while 
-- scopeCheck ["x", "y"] (Var "z") is False.
scopeCheckAux :: [Variable] -> ABLExpr -> Bool
scopeCheckAux vars (Val _) = True
scopeCheckAux vars (Var x) =
  case vars of
       [] -> False
       (v : vars) -> if v == x then True else scopeCheckAux vars (Var x)
scopeCheckAux vars (Add e1 e2) = (scopeCheckAux vars e1) && (scopeCheckAux vars e2)
scopeCheckAux vars (Sub e1 e2) = (scopeCheckAux vars e1) && (scopeCheckAux vars e2)
scopeCheckAux vars (Mul e1 e2) = (scopeCheckAux vars e1) && (scopeCheckAux vars e2)
scopeCheckAux vars (Div e1 e2) = (scopeCheckAux vars e1) && (scopeCheckAux vars e2)
scopeCheckAux vars (Eq e1 e2) = (scopeCheckAux vars e1) && (scopeCheckAux vars e2)
scopeCheckAux vars (And e1 e2) = (scopeCheckAux vars e1) && (scopeCheckAux vars e2)
scopeCheckAux vars (Or e1 e2) = (scopeCheckAux vars e1) && (scopeCheckAux vars e2)
scopeCheckAux vars (Not e) = scopeCheckAux vars e
scopeCheckAux vars (Let1 x e1 e2) = (scopeCheckAux vars e1) && (scopeCheckAux (x : vars) e2)
scopeCheckAux vars (If e1 e2 e3) = (scopeCheckAux vars e1) && (scopeCheckAux vars e2) && (scopeCheckAux vars e2)
scopeCheckAux vars (Fresh e) = scopeCheckAux [] e
scopeCheckAux vars (LetStar vs e) = scopeCheckAux vars (unfoldLetStar vs e)


-- Helper function to express a series of bindings as a nested Let1 expression.
unfoldLetStar :: [(Variable, ABLExpr)] -> ABLExpr -> ABLExpr
unfoldLetStar [] e = e
unfoldLetStar ((x, ex) : bindings) e = (Let1 x ex (unfoldLetStar bindings e))


-- add your tests here
tests :: IO ()
tests = do
  test "eval empty 10" 
       (evalABL empty (Val (Num 10))) 
       (Just (Num 10))
  test "eval (+ 11 12)" 
       (evalABL empty (Add (Val (Num 11)) (Val (Num 12))))
       (Just (Num 23))
  test "eval (- 11 12)"
       (evalABL empty (Sub (Val (Num 11)) (Val (Num 12))))
       (Just (Num (-1)))
  test "eval (- x 1)"
       (evalABL empty (Sub (Var "x") (Val (Num 1))))
       Nothing
  test "eval (* 1 0)"
       (evalABL empty (Mul (Val (Num 1)) (Val (Num 0))))
       (Just (Num 0))
  test "eval (* 2 3)"
       (evalABL empty (Mul (Val (Num 2)) (Val (Num 3))))
       (Just (Num 6))
  test "eval (/ 2 0)"
       (evalABL empty (Div (Val (Num 2)) (Val (Num 0))))
       Nothing
  test "eval (/ 2 3)"
       (evalABL empty (Div (Val (Num 2)) (Val (Num 3))))
       (Just (Num 0))
  test "eval (/ 6 3)"
       (evalABL empty (Div (Val (Num 6)) (Val (Num 3))))
       (Just (Num 2))
  test "eval (/ 2 x)"
       (evalABL [("x", (Num 1))] (Div (Val (Num 2)) (Var "x")))
       (Just (Num 2))
  test "eval (= 1 1)"
       (evalABL empty (Eq (Val (Num 1)) (Val (Num 1))))
       (Just (Bool True))
  test "eval (= 0 1)"
       (evalABL empty (Eq (Val (Num 0)) (Val (Num 1))))
       (Just (Bool False))
  test "eval (= true true)"
       (evalABL empty (Eq (Val (Bool True)) (Val (Bool True))))
       (Just (Bool True))
  test "eval (= false true)"
       (evalABL empty (Eq (Val (Bool False)) (Val (Bool True))))
       (Just (Bool False))
  test "eval (= 0 false)"
       (evalABL empty (Eq (Val (Num 0)) (Val (Bool False))))
       Nothing
  test "eval (= true 1)"
       (evalABL empty (Eq (Val (Bool True)) (Val (Num 1))))
       Nothing
  test "eval (and true true)"
       (evalABL empty (And (Val (Bool True)) (Val (Bool True))))
       (Just (Bool True))
  test "eval (and true false)"
       (evalABL empty (And (Val (Bool True)) (Val (Bool False))))
       (Just (Bool False))
  test "eval (and false false)"
       (evalABL empty (And (Val (Bool False)) (Val (Bool False))))
       (Just (Bool False))
  test "eval (and false false)"
       (evalABL empty (And (Val (Bool False)) (Val (Bool False))))
       (Just (Bool False))
  test "eval (and 0 0)"
       (evalABL empty (And (Val (Num 0)) (Val (Num 0))))
       Nothing
  test "eval (and true 0)"
       (evalABL empty (And (Val (Bool True)) (Val (Num 0))))
       Nothing
  test "eval (or true false)"
       (evalABL empty (Or (Val (Bool True)) (Val (Bool False))))
       (Just (Bool True))
  test "eval (or false true)"
       (evalABL empty (Or (Val (Bool False)) (Val (Bool True))))
       (Just (Bool True))
  test "eval (or false false)"
       (evalABL empty (Or (Val (Bool False)) (Val (Bool False))))
       (Just (Bool False))
  test "eval (or 0 0)"
       (evalABL empty (Or (Val (Num 0)) (Val (Num 0))))
       Nothing
  test "eval (or true 0)"
       (evalABL empty (Or (Val (Bool True)) (Val (Num 0))))
       Nothing
  test "eval (not false)"
       (evalABL empty (Not (Val (Bool False))))
       (Just (Bool True))
  test "eval (not true)"
       (evalABL empty (Not (Val (Bool True))))
       (Just (Bool False))
  test "eval (not 0)"
       (evalABL empty (Not (Val (Num 0))))
       Nothing
  test "eval [] (let1 (x 1) (+ x 2))"
       (evalABL empty (Let1 "x" (Val (Num 1)) (Add (Var "x") (Val (Num 2)))))
       (Just (Num 3))
  test "eval [(x, 1)] (let1 (x 2) (+ x 2))"
       (evalABL [("x", (Num 1))] (Let1 "x" (Val (Num 2)) (Add (Var "x") (Val (Num 2)))))
       (Just (Num 4))
  test "eval [(x, 1)] (let1 (x (+ x 2)) (+ x 2))"
       (evalABL [("x", (Num 1))] (Let1 "x" (Add (Var "x") (Val (Num 2))) (Add (Var "x") (Val (Num 2)))))
       (Just (Num 5))
  test "eval [] (let1 (x y) (+ x 2))"
       (evalABL empty (Let1 "x" (Var "y") (Add (Var "x") (Val (Num 2)))))
       Nothing
  test "eval (if-else true 1 2)"
       (evalABL empty (If (Val (Bool True)) (Val (Num 1)) (Val (Num 2))))
       (Just (Num 1))
  test "eval (if-else false 1 2)"
       (evalABL empty (If (Val (Bool False)) (Val (Num 1)) (Val (Num 2))))
       (Just (Num 2))
  test "eval [(x, 1)] (if-else (= x 1) 1 0)"
       (evalABL [("x", (Num 1))] (If (Eq (Var "x") (Val (Num 1))) (Val (Num 1)) (Val (Num 0))))
       (Just (Num 1))
  test "eval [(x, 1)] (if-else (= x 2) 1 0)"
       (evalABL [("x", (Num 1))] (If (Eq (Var "x") (Val (Num 2))) (Val (Num 1)) (Val (Num 0))))
       (Just (Num 0))
  test "eval [(x, 1)] (if-else x 1 0)"
       (evalABL [("x", (Num 1))] (If (Var "x") (Val (Num 1)) (Val (Num 0))))
       Nothing
  test "scopeCheck (Var x)"
       (scopeCheck (Var "x"))
       False
  test "scopeCheck (Let1 (x 1) x)"
       (scopeCheck (Let1 "x" (Val (Num 1)) (Var "x")))
       True
  test "scopeCheck (Let1 (x 2) (+ x y))"
       (scopeCheck (Let1 "x" (Val (Num 2)) (Add (Var "x") (Var "y"))))
       False
  test "scopeCheck (Add x y)"
       (scopeCheck (Add (Var "x") (Var "y")))
       False
  test "scopeCheck (Let x 1 (- 2 (+ x 1)))"
       (scopeCheck (Let1 "x" (Val (Num 1)) (Sub (Val (Num 2)) (Add (Var "x") (Val (Num 1))))))
       True
  test "scopeCheck (Let x 1 (Fresh (Let x 2 (+ x 1))))"
       (scopeCheck (Let1 "x" (Val (Num 1)) (Fresh (Let1 "x" (Val (Num 2)) (Add (Var "x") (Val (Num 1)))))))
       True
  test "scopeCheck (Let x 1 (Fresh (+ x 1)))"
       (scopeCheck (Let1 "x" (Val (Num 1)) (Fresh (Add (Var "x") (Val (Num 1))))))
       False
  test "scopeCheck (Let x 1 (Fresh (Let y 2 (+ x y))))"
       (scopeCheck (Let1 "x" (Val (Num 1)) (Fresh (Let1 "y" (Val (Num 2)) (Add (Var "x") (Val (Num 1)))))))
       False
  test "scopeCheck (let* (x 1) x)"
       (scopeCheck (LetStar [("x", (Val (Num 1)))] (Var "x")))
       True
  test "scopeCheck (let* () y)"
       (scopeCheck (LetStar [] (Var "y")))
       False
  test "scopeCheck (let* (x 1) y)"
       (scopeCheck (LetStar [("x", (Val (Num 1)))] (Var "y")))
       False
  test "evalABL (let* () true)"
       (evalABL empty (LetStar [("x", (Val (Num 1)))] (Val (Bool True))))
       (Just (Bool True))
  test "evalABL (let* (x 1) y)"
       (evalABL empty (LetStar [("x", (Val (Num 1)))] (Var "y")))
       Nothing
  test "evalABL (let* ((x 1)(y 2)) (+ x y))"
       (evalABL empty (LetStar [("x", (Val (Num 1))), ("y", (Val (Num 2)))] (Add (Var "y") (Var "x"))))
       (Just (Num 3))
  test "evalABL (let* ((x 1) (y (+ x 2))) (+ x y))"
       (evalABL empty (LetStar [("x", (Val (Num 1))), ("y", (Add (Var "x") (Val (Num 2))))] (Add (Var "y") (Var "x"))))
       (Just (Num 4))

---------------------------- your helper functions --------------------------
