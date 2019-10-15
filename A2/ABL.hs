{- |
Module      :  ABL
Description :  Syntax of the ABL language.

Maintainer  :  Zerun Tian <tian.ze@husky.neu.edu>
-}

module ABL where

import SimpleTests

-- We represent variables as strings and introduce `Variable` as an alias for
-- `String`.
type Variable = String

-- ABL values are either numbers (integers) or booleans.
data ABLValue = Num Integer
              | Bool Bool
              deriving (Show, Eq)

data ABLExpr = Var Variable
             | Val ABLValue
             | Add ABLExpr ABLExpr
             | Sub ABLExpr ABLExpr
             | Mul ABLExpr ABLExpr
             | Div ABLExpr ABLExpr
             | Eq ABLExpr ABLExpr
             | And ABLExpr ABLExpr
             | Or ABLExpr ABLExpr
             | Not ABLExpr
             | Let1 Variable ABLExpr ABLExpr
             | If ABLExpr ABLExpr ABLExpr
             | Fresh ABLExpr
             | LetStar [(Variable, ABLExpr)] ABLExpr
             deriving (Show, Eq)

showABL :: ABLExpr -> String
showABL (Var x) = x
showABL (Val (Num n)) = show n
showABL (Val (Bool b)) =
  if b
     then "true"
     else "false"
showABL (Add e1 e2) = "(+ " ++ showABL e1 ++ " " ++ showABL e2 ++ ")"
showABL (Sub e1 e2) = "(- " ++ showABL e1 ++ " " ++ showABL e2 ++ ")"
showABL (Mul e1 e2) = "(* " ++ showABL e1 ++ " " ++ showABL e2 ++ ")"
showABL (Div e1 e2) = "(/ " ++ showABL e1 ++ " " ++ showABL e2 ++ ")"
showABL (Eq e1 e2) = "(= " ++ showABL e1 ++ " " ++ showABL e2 ++ ")"
showABL (And e1 e2) = "(&& " ++ showABL e1 ++ " " ++ showABL e2 ++ ")"
showABL (Or e1 e2) = "(|| " ++ showABL e1 ++ " " ++ showABL e2 ++ ")"
showABL (Not e) = "(not " ++ showABL e ++ ")"
showABL (Let1 v e1 e2) = "(let1 (" ++ v ++ " " ++ showABL e1 ++ ") " ++ showABL e2 ++ ")"
showABL (If e1 e2 e3) = "(if-else " ++ showABL e1 ++ " " ++ showABL e2 ++ " " ++ showABL e3 ++ ")"
showABL (Fresh e) = "(fresh-env " ++ showABL e ++ ")"
showABL (LetStar vars e) = "(let* (" ++ showVarList vars ++ ") " ++ showABL e ++ ")"


-- add tests
tests :: IO ()
tests = do
  test "showABL num"
       (showABL (Val (Num 10)))
       "10"
  test "showABL true"
       (showABL (Val (Bool True)))
       "true"
  test "showABL false"
       (showABL (Val (Bool False)))
       "false"
  test "showABL addition"
       (showABL (Add (Val (Num 1)) (Val (Num 2))))
       "(+ 1 2)"
  test "showABL subtraction"
       (showABL (Sub (Val (Num 2)) (Val (Num 1))))
       "(- 2 1)"
  test "showABL multiplication"
       (showABL (Mul (Val (Num 1)) (Val (Num 2))))
       "(* 1 2)"
  test "showABL division"
       (showABL (Div (Val (Num 2)) (Val (Num 1))))
       "(/ 2 1)"
  test "showABL equality"
       (showABL (Eq (Val (Bool True)) (Val (Bool True))))
       "(= true true)"
  test "showABL and"
       (showABL (And (Val (Bool True)) (Val (Bool True))))
       "(&& true true)"
  test "showABL or"
       (showABL (Or (Val (Bool False)) (Val (Bool True))))
       "(|| false true)"
  test "showABL not"
       (showABL (Not (Val (Bool False))))
       "(not false)"
  test "showABL let1"
       (showABL (Let1 "x" (Val (Num 1))
                      (Add (Var "x") (Val (Num 2)))))
       "(let1 (x 1) (+ x 2))"
  test "showABL if"
       (showABL (If (Not (Val (Bool False)))
                    (Val (Num 1))
                    (Val (Num 2))))
       "(if-else (not false) 1 2)"
  test "showABL fresh-env"
       (showABL (Fresh (Var "x")))
       "(fresh-env x)"
  test "showABL LetStar []"
       (showABL (LetStar [] (Var "x")))
       "(let* () x)"
  test "showABL LetStar x"
       (showABL (LetStar [("x", (Val (Num 1)))] (Var "x")))
       "(let* ((x 1)) x)"
  test "showABL LetStar x and y"
       (showABL (LetStar [("x", (Val (Num 1))), ("y", (Val (Bool True)))] (Var "z")))
       "(let* ((x 1) (y true)) z)"

---------------------------- your helper functions --------------------------
showVarList :: [(Variable, ABLExpr)] -> String
showVarList [] = ""
showVarList ((v, e) : rest) = "(" ++ v ++ " " ++ showABL e ++ ")" ++
                              (if not (rest == []) then " " else "") ++ showVarList rest
