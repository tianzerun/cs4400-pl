{- |
Module      :  Syntax
Description :  Syntax of a Simply Typed Lambda Calculus with extensions.

Maintainer  :  Ferd <f.vesely@northeastern.edu>
-}
module Syntax where

import Maps

import Debug.Trace (trace)

type Variable = String

data Value = Num Integer
           | Bool Bool
           -- the values below should not be typable
           | Clo Variable Expr Env
           | VPair Value Value
           | VCons Value Value
           | VNil 
           deriving (Show, Eq)

data Expr = Val Value
          -- basic Simply Typed Lambda
          | Var Variable           -- variable
          | Lam Variable Type Expr -- lambda abstraction
          | App Expr Expr          -- application
          | Fix Expr               -- fixed point operator
          -- Familiar extensions
          | Let Variable Expr Expr -- simple let binding
          | Add Expr Expr          -- arithmetic expressions
          | Sub Expr Expr
          | Mul Expr Expr
          | And Expr Expr          -- boolean expressions
          | Not Expr
          | Leq Expr Expr          -- less than or equal predicate
          | If Expr Expr Expr      -- conditionals
        
          -- New extensions
          | Pair Expr Expr         -- pairs
          | Fst Expr               -- select the left element of a pair
          | Snd Expr               -- select the right element of a pair

          | Cons Expr Expr         -- lists:
          | Nil Type               -- empty list
          | IsNil Expr             -- is the list empty?
          | Head Expr              -- head of the list
          | Tail Expr              -- tail of the list
          deriving (Show, Eq)

data Type = TyInt
          | TyBool
          | TyArrow Type Type
          | TyPair Type Type
          | TyList Type
          deriving (Show, Eq)

type Env = Map Variable Value
type TEnv = Map Variable Type

-- shorthands for injecting integers and booleans into Expr
num n = Val (Num n)
bool b = Val (Bool b)


