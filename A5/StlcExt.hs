{- |
Module      :  StlcExt
Description :  Implementation of answers to Exercises 4-7.

Maintainer  :  Zerun Tian <tian.ze@husky.neu.edu>
-}

{-# OPTIONS_GHC -fdefer-typed-holes -fwarn-incomplete-patterns #-}
module StlcExt where

import Syntax
import Eval
import Types

import Maps

import SimpleTests

-- **Exercise 4**
-- And True (If True then False else 1)
untypedButOk1 :: Expr
untypedButOk1 = And (bool True) (If (bool True) (bool False) (num 1))

-- Head [1, False]
untypedButOk2 :: Expr
untypedButOk2 = Head (Cons (num 1) (Cons (bool False) (Nil TyBool)))

-- App (Lam x. TyBool (x + 1)) 0
untypedButOk3 :: Expr
untypedButOk3 = (App (Lam "x" TyBool (Add (Var "x") (num 1))) (num 0))


-- For simplifying definitions of functions, you might find it worthwhile to
-- complete the following function. It should take a function name, a list of 
-- pairs of arguments names with their types, a return type and the body.
defineFun :: Variable -> [(Variable, Type)] -> Type -> Expr -> Expr
defineFun f args returnType body = 
  Fix (Lam f funType inner)
  where funType = flatArgsType args returnType
        inner = expandArgs args body


-- Example function definitions:

-- 1. Summing up the first n natural numbers
sumFirstN = 
  Fix (Lam "sum" (TyArrow TyInt TyInt) (Lam "n" TyInt (
    If (Leq (Var "n") (num 0))
       (num 0)
       (Add (Var "n") (App (Var "sum") (Sub (Var "n") (num 1)))))))

-- 2. Factorial
factorialExpr :: Expr
factorialExpr = Fix (
  Lam "f" (TyArrow TyInt TyInt) (
  Lam "x" TyInt (
    If (Leq (Var "x") (num 0))
       (num 1)
       (Mul (Var "x") (App (Var "f") (Sub (Var "x") (num 1))))
  )))

-- The same, using defineFun - if implemented
factorialExpr' :: Expr
factorialExpr' = 
  defineFun "f" [("x", TyInt)] TyInt (
    If (Leq (Var "x") (num 0))
       (num 1)
       (Mul (Var "x") (App (Var "f") (Sub (Var "x") (num 1))))
  )




-- Exercise 5
swapExpr :: Expr
swapExpr =
  defineFun "f" [("p", TyPair TyBool TyInt)] (TyPair TyInt TyBool) (
    Pair (Snd (Var "p")) (Fst (Var "p"))
  )

swapExprType :: Type
swapExprType = TyArrow (TyPair TyBool TyInt) (TyPair TyInt TyBool)


-- Exercise 6
boolListLengthExpr :: Expr
boolListLengthExpr =
   defineFun "f" [("l", TyList TyBool)] TyInt (
    If (IsNil (Var "l"))
       (num 0)
       (Add (num 1) (App (Var "f") (Tail (Var "l"))))
   )

boolListLengthExprType :: Type
boolListLengthExprType = TyArrow (TyList TyBool) TyInt


-- Exercise 7
zipIntExpr :: Expr
zipIntExpr =
  defineFun "f" [("p", TyPair (TyList TyInt) (TyList TyBool))] (TyList (TyPair TyInt TyBool)) (
    If (IsNil (Fst (Var "p")))
       (Nil (TyPair TyInt TyBool))
       (If (IsNil (Snd (Var "p")))
           (Nil (TyPair TyInt TyBool))
           (Cons (Pair (Head (Fst (Var "p"))) (Head (Snd (Var "p"))))
                 (App (Var "f") (Pair (Tail (Fst (Var "p"))) (Tail (Snd (Var "p")))))))
  )

zipIntExprType :: Type
zipIntExprType = TyArrow (TyPair (TyList TyInt) (TyList TyBool)) (TyList (TyPair TyInt TyBool))


---------------------------- your helper functions --------------------------
flatArgsType :: [(Variable, Type)] -> Type -> Type
flatArgsType [] returnType = returnType
flatArgsType ((_, t) : l) returnType = (TyArrow t (flatArgsType l returnType))

expandArgs :: [(Variable, Type)] -> Expr -> Expr
expandArgs [] body = body
expandArgs ((v, t) : l) body = Lam v t (expandArgs l body)


----------------------------------- TESTS -----------------------------------
list1 = Cons (num 1) (Cons (num 2) (Cons (num 3) (Cons (num 4) (Nil TyInt))))
list2 = Cons (bool True) (Cons (bool False) (Cons (bool False) (Nil TyBool)))
list3 = Cons (num 1) (Cons (num 2) (Nil TyInt))

tests :: IO ()
tests = do
  test "example typeOf test" 
    (typeOf empty sumFirstN) 
    (Just (TyArrow TyInt TyInt))
  test "example eval test" 
    (eval empty (App sumFirstN (num 5))) 
    (Just (Num 15))
  test "untypedButOk1 has no type" (typeOf empty untypedButOk1) Nothing
  test "untypedButOk2 has no type" (typeOf empty untypedButOk2) Nothing
  test "untypedButOk3 has no type" (typeOf empty untypedButOk3) Nothing
  test "untypedButOk1 can be evaluated" (eval empty untypedButOk1) (Just (Bool False))
  test "untypedButOk2 can be evaluated" (eval empty untypedButOk2) (Just (Num 1))
  test "untypedButOk3 can be evaluated" (eval empty untypedButOk3) (Just (Num 1))
  -- test cases for swapExpr
  test "|- swapExpr : swapExprType"
       (typeOf empty swapExpr)
       (Just swapExprType)
  test "eval swapExpr (False, 3) => (3, False)"
       (eval empty (App swapExpr (Pair (bool False) (num 3))))
       (Just (VPair (Num 3) (Bool False)))
  test "eval swapExpr (3, False) => (False, 3)"
       (eval empty (App swapExpr (Pair (num 3) (bool False))))
       (Just (VPair (Bool False) (Num 3)))
  test "|- swapExpr (False, 3) : (TyInt, TyBool)"
       (typeOf empty (App swapExpr (Pair (bool False) (num 3))))
       (Just (TyPair TyInt TyBool))
  test "|- swapExpr (3, False) : Nothing"
       (typeOf empty (App swapExpr (Pair (num 3) (bool False))))
       Nothing
  -- test cases for boolListLengthExpr
  test "|- boolListLengthExpr : boolListLengthExprType"
       (typeOf empty boolListLengthExpr)
       (Just boolListLengthExprType)
  test "eval boolListLengthExpr [True, False, False] => 3"
       (eval empty
             (App boolListLengthExpr
                  (Cons (bool True) (Cons (bool False) (Cons (bool False) (Nil TyBool))))))
       (Just (Num 3))
  test "eval boolListLengthExpr [1, 2, 3] => 3"
       (eval empty
             (App boolListLengthExpr
                  (Cons (num 1) (Cons (num 2) (Cons (num 3) (Nil TyInt))))))
       (Just (Num 3))
  test "|- boolListLengthExpr [True, False, False] : TyInt"
       (typeOf empty
             (App boolListLengthExpr
                  (Cons (bool True) (Cons (bool False) (Cons (bool False) (Nil TyBool))))))
       (Just TyInt)
  test "|- boolListLengthExpr [1, 2, 3] : Nothing"
       (typeOf empty
               (App boolListLengthExpr
                    (Cons (num 1) (Cons (num 2) (Cons (num 3) (Nil TyInt))))))
       Nothing
  -- test cases for zipIntExpr
  test "|- zipIntExpr : zipIntExprType"
       (typeOf empty zipIntExpr)
       (Just zipIntExprType)
  test "eval zipIntExpr (list1 list2) => [(1, True), (2, False), (3, False)]"
       (eval empty (App zipIntExpr (Pair list1 list2)))
       (Just (VCons (VPair (Num 1) (Bool True))
                    (VCons (VPair (Num 2) (Bool False))
                           (VCons (VPair (Num 3) (Bool False)) VNil))))
  test "|- zipIntExpr (list1 list2) : TyList (TyPair TyInt TyBool)"
       (typeOf empty (App zipIntExpr (Pair list1 list2)))
       (Just (TyList (TyPair TyInt TyBool)))
  test "|- zipIntExpr (list2 list1) : TyList (TyPair TyInt TyBool)"
       (typeOf empty (App zipIntExpr (Pair list2 list1)))
       Nothing
  test "eval zipIntExpr (list2 list1) => [(True, 1), (False, 2), (False, 3)]"
       (eval empty (App zipIntExpr (Pair list2 list1)))
       (Just (VCons (VPair (Bool True) (Num 1))
                    (VCons (VPair (Bool False) (Num 2))
                           (VCons (VPair (Bool False) (Num 3)) VNil))))
  test "eval zipIntExpr (list3 list2) => [(1, True), (2, False)]"
       (eval empty (App zipIntExpr (Pair list3 list2)))
       (Just (VCons (VPair (Num 1) (Bool True))
                    (VCons (VPair (Num 2) (Bool False)) VNil)))
