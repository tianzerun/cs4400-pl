{- |
Module      :  Eval
Description :  Evaluator for a Simply Typed Lambda Calculus with extensions.

Maintainer  :  Ferd <f.vesely@northeastern.edu>
-}


{-# OPTIONS_GHC -fdefer-typed-holes -fwarn-incomplete-patterns #-}
module Eval where

import Syntax

import Maps

import Debug.Trace (trace)

----------------------------------- evaluator -------------------------------

eval :: Env -> Expr -> Maybe Value
eval env (Val v) = Just v
eval env (Var x) = get x env
eval env (Lam x _ e) = 
  Just (Clo x e env)
eval env (App e1 e2) =
  do Clo x e env' <- eval env e1
     v2 <- eval env e2
     eval (add x v2 env') e 
eval env (Fix e) =
  do Clo f e' env' <- eval env e
     eval (add f (Clo "x" (App (Fix e) (Var "x")) env) env') e'
eval env (Add e1 e2) =
  do Num n1 <- eval env e1
     Num n2 <- eval env e2
     return (Num (n1 + n2))
eval env (Sub e1 e2) =
  do Num n1 <- eval env e1
     Num n2 <- eval env e2
     return (Num (n1 - n2))
eval env (Mul e1 e2) =
  do Num n1 <- eval env e1
     Num n2 <- eval env e2
     return (Num (n1 * n2))
eval env (If e1 e2 e3) =
  do Bool b1 <- eval env e1
     if b1 
        then eval env e2
        else eval env e3
eval env (And e1 e2) = 
  do Bool b1 <- eval env e1
     Bool b2 <- eval env e2
     return (Bool (b1 && b2))
eval env (Not e) = 
  do Bool b <- eval env e
     return (Bool (not b))
eval env (Leq e1 e2) =
  do Num n1 <- eval env e1
     Num n2 <- eval env e2
     return (Bool (n1 <= n2))
eval env (Let x e1 e2) =
  do v1 <- eval env e1
     eval (add x v1 env) e2
eval env (Pair e1 e2) =
  do v1 <- eval env e1
     v2 <- eval env e2
     return (VPair v1 v2)
eval env (Fst e) =
  do VPair v _ <- eval env e
     return v
eval env (Snd e) =
  do VPair _ v <- eval env e
     return v
eval env (Cons e1 e2) =
  do v1 <- eval env e1
     v2 <- eval env e2
     return (VCons v1 v2)
eval env (Nil _) = return VNil
eval env (Head e) =
  do VCons v _ <- eval env e
     return v
eval env (Tail e) =
  do VCons _ v <- eval env e
     return v
eval env (IsNil e) =
  case eval env e of
       Just VNil -> return (Bool True)
       Just (VCons _ _) -> return (Bool False)
       _ -> Nothing


