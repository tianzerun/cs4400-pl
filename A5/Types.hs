{- |
Module      :  Types
Description :  Type-checker implementation.

Maintainer  :  Ferd <f.vesely@northeastern.edu>
               Zerun Tian <tian.ze@husky.neu.edu>
-}

{-# OPTIONS_GHC -fdefer-typed-holes -fwarn-incomplete-patterns #-}
module Types where

import Syntax

import Maps

import Debug.Trace (trace)

import SimpleTests


-- complete the definition below
typeOf :: TEnv -> Expr -> Maybe Type
typeOf tenv (Val (Bool _)) = return TyBool
typeOf tenv (Val (Num _)) = return TyInt
typeOf tenv e@(Val _) = fail ("Runtime-only value " ++ show e)
typeOf tenv (Var x) = get x tenv
typeOf tenv (Lam x t1 e) = 
  do t2 <- typeOf (add x t1 tenv) e
     return (TyArrow t1 t2)
typeOf tenv e@(App e1 e2) =
  do TyArrow t2 t1 <- typeOf tenv e1
     t2' <- typeOf tenv e2
     if t2 == t2'
        then return t1
        else fail "App"
typeOf tenv (Fix e) =
  do TyArrow t1 t2 <- typeOf tenv e
     if t1 == t2 
        then return t1
        else fail "Fix"
typeOf tenv (Let x e1 e2) =
  do t1 <- typeOf tenv e1
     t2 <- typeOf (add x t1 tenv) e2
     return t2
typeOf tenv (Add e1 e2) =
  do TyInt <- typeOf tenv e1
     TyInt <- typeOf tenv e2
     return TyInt
typeOf tenv (Sub e1 e2) =
  do TyInt <- typeOf tenv e1
     TyInt <- typeOf tenv e2
     return TyInt
typeOf tenv (Mul e1 e2) =
  do TyInt <- typeOf tenv e1
     TyInt <- typeOf tenv e2
     return TyInt
typeOf tenv (And e1 e2) =
  do TyBool <- typeOf tenv e1
     TyBool <- typeOf tenv e2
     return TyBool
typeOf tenv (Not e) =
  do TyBool <- typeOf tenv e
     return TyBool
typeOf tenv (Leq e1 e2) =
  do TyInt <- typeOf tenv e1
     TyInt <- typeOf tenv e2
     return TyBool
typeOf tenv (If e1 e2 e3) =
  do TyBool <- typeOf tenv e1
     t1 <- typeOf tenv e2
     t2 <- typeOf tenv e3
     if t1 == t2
        then return t1
        else fail "If"
typeOf tenv (Pair e1 e2) =
  do t1 <- typeOf tenv e1
     t2 <- typeOf tenv e2
     return (TyPair t1 t2)
typeOf tenv (Fst e) =
  do TyPair t1 _ <- typeOf tenv e
     return t1
typeOf tenv (Snd e) =
  do TyPair _ t2 <- typeOf tenv e
     return t2
typeOf tenv (Nil t) = return (TyList t)
typeOf tenv (Cons e1 e2) =
  do t1 <- typeOf tenv e1
     TyList t2 <- typeOf tenv e2
     if t1 == t2
        then return (TyList t1)
        else fail "Cons"
typeOf tenv (Head e) =
  do TyList t <- typeOf tenv e
     return t
typeOf tenv (Tail e) =
  do TyList t <- typeOf tenv e
     return (TyList t)
typeOf tenv (IsNil e) =
  do TyList _ <- typeOf tenv e
     return TyBool


---------------------------- your helper functions --------------------------


----------------------------------- TESTS -----------------------------------
-- test assets
intIntPair :: Expr
intIntPair = (Pair (num 0) (num 1))

numList1 :: Expr
numList1 = (Cons (num 1) (Cons (num 2) (Nil TyInt)))

boolList1 :: Expr
boolList1 = (Cons (bool True) (Cons (bool False) (Nil TyBool)))

afList1 :: Expr
afList1 = Cons (Lam "x" TyInt (Leq (Var "x") (num 0))) (Nil (TyArrow TyInt TyBool))

-- test cases
tests :: IO ()
tests = do
  test "|- 4 + 5 : TyInt" (typeOf empty (Add (num 4) (num 5))) (Just TyInt)
  test "|- Let x 1 x : TyInt"
       (typeOf empty (Let "x" (num 1) (Var "x")))
       (Just TyInt)
  test "|- Let x True (Add 1 x) : Nothing"
       (typeOf empty (Let "x" (bool True) (Add (num 1) (Var "x"))))
       Nothing
  test "|- 4 - 5 : TyInt"
       (typeOf empty (Sub (num 4) (num 5)))
       (Just TyInt)
  test "|- 4 * 5 : TyInt"
       (typeOf empty (Mul (num 4) (num 5)))
       (Just TyInt)
  test "|- 4 * True : Nothing"
       (typeOf empty (Mul (num 4) (bool True)))
       Nothing
  test "|- False * True : Nothing"
       (typeOf empty (Mul (bool False) (bool True)))
       Nothing
  test "|- True && False : TyBool"
       (typeOf empty (And (bool True) (bool False)))
       (Just TyBool)
  test "|- !True: TyBool"
       (typeOf empty (Not (bool True)))
       (Just TyBool)
  test "|- !False: TyBool"
       (typeOf empty (Not (bool False)))
       (Just TyBool)
  test "|- !1: TyBool"
       (typeOf empty (Not (num 1)))
       Nothing
  test "|- 0 < 2 : TyBool"
       (typeOf empty (Leq (num 0) (num 2)))
       (Just TyBool)
  test "|- if True 1 2 : TyInt"
       (typeOf empty (If (bool True) (num 1) (num 2)))
       (Just TyInt)
  test "|- if False False True : TyBool"
       (typeOf empty (If (bool False) (bool False) (bool True)))
       (Just TyBool)
  test "|- if False 1 True : TyBool"
       (typeOf empty (If (bool False) (num 1) (bool True)))
       Nothing
  test "|- (1, True) : TyPair TyInt TyBool"
       (typeOf empty (Pair (num 1) (bool True)))
       (Just (TyPair TyInt TyBool))
  test "|- (True, 1) : TyPair TyBool TyInt"
       (typeOf empty (Pair (bool True) (num 1)))
       (Just (TyPair TyBool TyInt))
  test "|- (0, 1) : TyPair TyInt TyInt"
       (typeOf empty intIntPair)
       (Just (TyPair TyInt TyInt))
  test "|- (False, False) : TyPair TyBool TyBool"
       (typeOf empty (Pair (bool False) (bool False)))
       (Just (TyPair TyBool TyBool))
  test "|- (1, !1) : Nothing"
       (typeOf empty (Pair (num 1) (Not (num 1))))
       Nothing
  test "|- ((false + 1, false), 1) : Nothing"
       (typeOf empty (Pair (Pair (Add (bool False) (num 1)) (bool False)) (num 1)))
       Nothing
  test "|- Fst (0, 1) : TyInt"
       (typeOf empty (Fst intIntPair))
       (Just TyInt)
  test "|- Snd (0, 1) : TyInt"
       (typeOf empty (Snd intIntPair))
       (Just TyInt)
  test "|- Snd (1, True) : TyBool"
       (typeOf empty (Snd (Pair (num 1) (bool True))))
       (Just TyBool)
  test "|- Fst (1, !1) : Nothing"
       (typeOf empty (Fst (Pair (num 1) (Not (num 1)))))
       Nothing
  test "|- Snd (1, !1) : Nothing"
       (typeOf empty (Snd (Pair (num 1) (Not (num 1)))))
       Nothing
  test "|- int[] : TyList TyInt"
       (typeOf empty (Nil TyInt))
       (Just (TyList TyInt))
  test "|- bool[] : TyList TyBool"
       (typeOf empty (Nil TyBool))
       (Just (TyList TyBool))
  test "|- [1, 2] : TyList TyInt"
       (typeOf empty numList1)
       (Just (TyList TyInt))
  test "|- [True, False] : TyList TyBool"
       (typeOf empty boolList1)
       (Just (TyList TyBool))
  test "|- [1, True] : Nothing"
       (typeOf empty (Cons (num 1) (Cons (bool True) (Nil TyInt))))
       Nothing
  test "|- [(Lam x TyBool !x)] : Nothing" -- list element type does not match
       (typeOf empty (Cons (Lam "x" TyBool (Not (Var "x"))) (Nil (TyArrow TyInt TyBool))))
       Nothing
  test "|- Head (bool[]) : TyBool"
       (typeOf empty (Head (Nil TyBool)))
       (Just TyBool)
  test "|- Head bool[] : TyInt"
       (typeOf empty (Head (Nil TyInt)))
       (Just TyInt)
  test "|- Head [1, 2] : TyInt"
       (typeOf empty (Head numList1))
       (Just TyInt)
  test "|- Tail bool[] : TyList TyInt"
       (typeOf empty (Tail (Nil TyInt)))
       (Just (TyList TyInt))
  test "|- Tail False : Nothing"
       (typeOf empty (Tail (bool False)))
       Nothing
  test "|- IsNil bool[] : TyBool"
       (typeOf empty (IsNil (Nil TyBool)))
       (Just TyBool)
  test "|- IsNil [True, False] : TyBool"
       (typeOf empty (IsNil boolList1))
       (Just TyBool)
  test "|- IsNil [1, 2] : TyBool"
       (typeOf empty (IsNil numList1))
       (Just TyBool)
  test "|- IsNil (0, 1) : Nothing"
       (typeOf empty (IsNil intIntPair))
       Nothing
