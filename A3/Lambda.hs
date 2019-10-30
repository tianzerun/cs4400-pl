{- |
Module      :  Lambda
Description :  Pure Lambda Calculus abstract syntax.

Maintainer  :  Ferd <f.vesely@northeastern.edu>
-}
module Lambda where

type Variable = String

data Lambda = Lam Variable Lambda
            | App Lambda Lambda
            | Var Variable
            deriving (Eq, Show)

-- pretty printing Lambda terms
showLambda (Var x) = x
showLambda (Lam x e) = "(λ" ++ x ++ ". " ++ showLambda e ++ ")"
showLambda (App e1 e2) = "(" ++ showLambda e1 ++ " " ++ showLambda e2 ++ ")"

-- If you want Haskell to use the above pretty-printer as default (e.g., in 
-- GHCi), remove Show from the `deriving` clasue above and uncomment the 
-- instance definition below.
 
{-
instance Show Lambda where
  show = showLambda
-}

