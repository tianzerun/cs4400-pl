{- |
Module      :  MiniImp
Description :  Implementation of the MiniImp language.

Maintainer  :  Zerun Tian <tian.ze@husky.neu.edu>
-}

module MiniImp where

import Store

import SimpleTests

import Debug.Trace (trace) -- useful for debugging purposes

type Variable = String

data Value = Num Integer
           | Bool Bool
           | Array [Value] -- replace with your representation of arrays
           deriving (Show, Eq)

data Expr = Val Value
          | Var String
          -- arithmetic expressions
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          -- boolean expressions
          | And Expr Expr
          | Not Expr
          -- less than or equal to
          | Le Expr Expr
          -- get the value stored in array at index
          | Get Variable Expr
          deriving (Show, Eq)

-- shorthands for values
num n = Val (Num n)
bool b = Val (Bool b)

data Stmt = Assign Variable Expr
          | Seq Stmt Stmt
          | While Expr Stmt
          | If Expr Stmt Stmt
          | Print Expr
          -- perform the body while the condition is true, but at least once
          | DoWhile Stmt Expr
          -- loop the specified number of times, using a counter variable
          | For Variable Expr Expr Stmt
          -- read a number from the input stream and store it in the given 
          -- variable
          | Read Variable
          -- allocate a new array
          | NewArray Variable Expr Expr
          -- set a value in array at index
          | Set Variable Expr Expr
          -- execute body for each 
          | ForEach Variable Variable Stmt
          -- for-loop with step specification
          deriving (Show, Eq)


type In = [Integer]
type Out = [Value]

-- expression evaluation (expressions)
evalExpr :: Store Value -> Expr -> Maybe Value
evalExpr _ (Val v) = return v
evalExpr sto (Var x) = get x sto
evalExpr sto (Add e1 e2) = 
  do Num n1 <- evalExpr sto e1
     Num n2 <- evalExpr sto e2
     return (Num (n1 + n2))
evalExpr sto (Sub e1 e2) = 
  do Num n1 <- evalExpr sto e1
     Num n2 <- evalExpr sto e2
     return (Num (n1 - n2))
evalExpr sto (Mul e1 e2) = 
  do Num n1 <- evalExpr sto e1
     Num n2 <- evalExpr sto e2
     return (Num (n1 * n2))
evalExpr sto (And e1 e2) = 
  do Bool b1 <- evalExpr sto e1
     Bool b2 <- evalExpr sto e2
     return (Bool (b1 && b2))
evalExpr sto (Not e) = 
  do Bool b <- evalExpr sto e
     return (Bool (not b))
evalExpr sto (Le e1 e2) = 
  do Num n1 <- evalExpr sto e1
     Num n2 <- evalExpr sto e2
     return (Bool (n1 <= n2))
-- complete the definition
evalExpr sto (Get x i) =
  do Array a <- evalExpr sto (Var x)
     Num n <- evalExpr sto i
     if (n >= 0) && (n < (fromIntegral (length a)))
        then return (a !! (fromIntegral n))
        else Nothing


-- evaluation of statements (commands)
execStmt :: (Stmt, Store Value, In) -> Maybe (Store Value, In, Out)
execStmt (Assign x e, sto, i) =
  do v <- evalExpr sto e
     return (add x v sto, i, [])
execStmt (Seq s1 s2, sto, i) = 
  do (sto', i', out1) <- execStmt (s1, sto, i)
     (sto'', i'', out2) <- execStmt (s2, sto', i')
     return (sto'', i'', out1 ++ out2)
-- complete the definition
execStmt (While e s, sto, i) =
  do (Bool b) <- evalExpr sto e
     if not b
        then return (sto, i, [])
        else do (sto', i', out1) <- execStmt (s, sto, i)
                (sto'', i'', out2) <- execStmt ((While e s), sto', i')
                return (sto'', i'', out1 ++ out2)
execStmt (If e s1 s2, sto, i) =
  do (Bool b) <- evalExpr sto e
     if b
        then execStmt (s1, sto, i)
        else execStmt (s2, sto, i)
execStmt (Print e, sto, i) =
  do v <- evalExpr sto e
     return (sto, i, [v])
execStmt (DoWhile b c, sto, i) =
  do (sto', i', out1) <- execStmt (b, sto, i)
     (Bool c') <- evalExpr sto' c
     if c'
        then do (sto'', i'', out2) <- execStmt (DoWhile b c, sto', i')
                return (sto'', i'', out1 ++ out2)
        else return (sto', i', out1)
execStmt (For x s e body, sto, i) = -- For Variable Expr Expr Stmt
  do (Num start) <- evalExpr sto s
     (Num end) <- evalExpr sto e
     if start <= end
        then do (sto'', i', out1) <- execStmt (body, (add x (Num start) sto), i)
                (sto''', i'', out2) <- execStmt (For x (num (start + 1)) e body, sto'', i')
                return (sto''', i'', out1 ++ out2)
        else return (sto, i, [])
execStmt (Read x, sto, i) = -- Read Variable (read value at the head of the list)
  if null i
     then Nothing
     else return ((add x (Num (head i)) sto), tail i, [])
execStmt (NewArray x s v, sto, i) = -- NewArray Variable Expr Expr
  do (Num size) <- evalExpr sto s
     v' <- evalExpr sto v
     return ((add x (Array (replicate (fromIntegral size) v')) sto), i, [])
execStmt (Set x n v, sto, i) = -- Set Variable Expr Expr
  do (Num index) <- evalExpr sto n
     value <- evalExpr sto v
     (Array arr) <- get x sto
     if (index >= 0) && (index < (fromIntegral (length arr)))
        then return ((add x (Array (replaceAtIndex arr index value)) sto), i, [])
        else Nothing
execStmt (ForEach x a b, sto, i) = -- ForEach Variable Variable Stmt
  execForEachStmtHelper (ForEach x a b, sto, i) 0


exercise6 :: Stmt
exercise6 = (NewArray "array" (num 5) (num 0)) `Seq`
            (For "i" (num 0) (num 4)
                 ((Read "x") `Seq`
                  (Set "array" (Var "i") (Var "x")))) `Seq`
            (Read "multiplier") `Seq`
            (ForEach "n" "array"
                     (Print (Mul (Var "multiplier") (Var "n"))))


exercise7 :: Stmt
exercise7 = (Assign "sum" (num 0)) `Seq`
            (Assign "greaterThanZero" (bool True)) `Seq`
            (While (Var "greaterThanZero")
                   ((Read "x") `Seq`
                    (If (Le (Var "x") (num 0))
                        (Assign "greaterThanZero" (bool False))
                        ((Assign "sum" (Add (Var "sum") (Var "x"))) `Seq` (Print (Var "sum"))))))


---------------------------- your helper functions --------------------------
replaceAtIndex :: [Value] -> Integer -> Value -> [Value]
replaceAtIndex [] _ _ = []
replaceAtIndex (v : vs) i nv | i == 0 = nv : vs
                             | otherwise = (v : replaceAtIndex vs (i - 1) nv)

execForEachStmtHelper :: (Stmt, Store Value, In) -> Integer -> Maybe (Store Value, In, Out)
execForEachStmtHelper (ForEach x a b, sto, i) index =
  do (Array arr) <- get a sto
     if (index < (fromIntegral (length arr)))
        then do v <- evalExpr sto (Get a (num index))
                (sto', i', out1) <- execStmt (b, (add x v sto), i)
                (sto'', i'', out2) <- execForEachStmtHelper (ForEach x a b, sto', i') (index + 1)
                return (sto'', i'', out1 ++ out2)
        else return (sto, i, [])
execForEachStmtHelper _ _ = Nothing
----------------------------------- TESTS -----------------------------------

-- Helpers for testing
-- Feel free to introduce further shorthands to help with testing
selectIn :: Maybe (Store a, In, Out) -> Maybe In
selectIn (Just (_, i, _)) = Just i
selectIn _ = Nothing

selectOut :: Maybe (Store a, In, Out) -> Maybe Out
selectOut (Just (_, _, o)) = Just o
selectOut _ = Nothing

cfgWithIn :: Stmt -> In -> (Stmt, Store Value, In) -- gives an empty store
cfgWithIn s i = (s, empty, i)

emptyCfg :: Stmt -> (Stmt, Store Value, In)
emptyCfg s = cfgWithIn s []

execToOutWithIn :: Stmt -> In -> Maybe Out
execToOutWithIn s i = selectOut (execStmt (cfgWithIn s i))

execToOut :: Stmt -> Maybe Out
execToOut s = execToOutWithIn s []

testEnv1 :: Store Value
testEnv1 = (fromList [("x", (Array [(Num 2), (Num 3)])), ("a", Num 0)])

tests :: IO ()
tests = do
  -- tests for evalExpr
  -- | Get
  test "x[0] from empty env"
       (evalExpr empty (Get "x" (num 0)))
       Nothing
  test "x = [2, 3] x[-1]"
       (evalExpr testEnv1 (Get "x" (num (-1))))
       Nothing
  test "x = [2, 3] x[0]"
       (evalExpr testEnv1 (Get "x" (num 0)))
       (Just (Num 2))
  test "x = [2, 3] x[1]"
       (evalExpr testEnv1 (Get "x" (num 1)))
       (Just (Num 3))
  test "x = [2, 3] x[2]"
       (evalExpr testEnv1 (Get "x" (num 2)))
       Nothing
  -- tests for execStmt
  -- | Assign, Seq, While, If, and Print
  test "assign x = 1 `seq` print x"
       (execToOut (Seq (Assign "x" (num 1)) (Print (Var "x"))))
       (Just [Num 1])
  test "assign x = true `seq` print x"
       (execToOut (Seq (Assign "x" (bool True)) (Print (Var "x"))))
       (Just [Bool True])
  test "assign x = [1] `seq` print x"
       (execToOut (Assign "x" (Val (Array [Num 1])) `Seq` (Print (Var "x"))))
       (Just [Array [Num 1]])
  test "assign x = True `seq` assign x = False `seq` print x"
       (execToOut ((Assign "x" (bool True)) `Seq` (Assign "x" (bool False)) `Seq` (Print (Var "x"))))
       (Just [Bool False])
  test "env={x:0}, assign x = 1 `seq` print x"
       (selectOut (execStmt (Assign "x" (num 1) `Seq` (Print (Var "x")), fromList [("x", (Num 0))], [])))
       (Just [Num 1])
  test "print 10"
       (execStmt (Print (Val (Num 10)), empty, [])) 
       (Just (empty, [], [Num 10]))
  test "env=empty, assign x = x + 1"
       (execToOut ((Assign "x" (Add (Var "x") (num 1)))))
       Nothing
  test "store = {x: [2, 3]} print x[1] print x[0]"
       (selectOut (execStmt ((Print (Get "x" (num 1))) `Seq` (Print (Get "x" (num 0))), testEnv1, [])))
       (Just [Num 3, Num 2])
  test "store = {x: [2, 3]} print x[0] print x[0]"
       (selectOut (execStmt ((Print (Get "x" (num 0))) `Seq` (Print (Get "x" (num 0))), testEnv1, [])))
       (Just [Num 2, Num 2])
  test "if true then print 1 else print 2"
       (execToOut (If (bool True) (Print (num 1)) (Print (num 2))))
       (Just [Num 1])
  test "if (True && False) then print 1 else print 2"
       (execToOut (If (And (bool True) (bool False)) (Print (num 1)) (Print (num 2))))
       (Just [Num 2])
  test "while false { print 12 }"
       (execToOut (While (bool False) (Print (num 12))))
       (Just [])
  test "env=testEnv1, while a <= 2 { a += 1 `seq` print a }"
       (selectOut (execStmt (While (Le (Var "a") (num 2))
                                   ((Assign "a" (Add (Var "a") (num 1))) `Seq`
                                    (Print (Var "a"))),
                             testEnv1, [])))
       (Just [Num 1, Num 2, Num 3])
  -- | DoWhile
  test "env=empty, do { print x } while false"
       (execToOut (DoWhile (Print (Var "x")) (bool False)))
       Nothing
  test "do { print 12 } while false"
       (execToOut (DoWhile (Print (num 12)) (bool False)))
       (Just [Num 12])
  test "env=testEnv1, do { a += 1 `seq` print a } while a <= 2"
       (selectOut (execStmt (DoWhile ((Assign "a" (Add (Var "a") (num 1))) `Seq`
                                      (Print (Var "a")))
                                     (Le (Var "a") (num 2)),
                             testEnv1, [])))
       (Just [Num 1, Num 2, Num 3])
  -- | For
  test "for x = 1 to 1 print x"
       (execToOut (For "x" (num 1) (num 1) (Print (Var "x"))))
       (Just [Num 1])
  test "for x = 2 to 1 print x"
       (execToOut (For "x" (num 2) (num 1) (Print (Var "x"))))
       (Just [])
  test "for x = 1 to 5 { print x }"
       (execToOut (For "x" (num 1) (num 5) (Print (Var "x"))))
       (Just [Num 1, Num 2, Num 3, Num 4, Num 5])
  test "for x = 0 to 2 { read in `seq` print (x * in) }"
       (execToOutWithIn (For "x" (num 0) (num 2) ((Read "in") `Seq` (Print (Mul (Var "x") (Var "in"))))) [1, 2, 3])
       (Just [Num 0, Num 2, Num 6])
  test "for i = 0 to 1 { print x[i] }"
       (selectOut (execStmt (For "i" (num 0) (num 1) (Print (Get "x" (Var "i"))),
                             testEnv1, [])))
       (Just [Num 2, Num 3])
  test "for i = 0 to 2 { print x[i] }" -- 2 is index out of bound
       (selectOut (execStmt (For "i" (num 0) (num 2) (Print (Get "x" (Var "i"))),
                             testEnv1, [])))
       Nothing
  -- | Read
  test "read x"
       (execToOutWithIn (Read "x") [])
       Nothing
  test "read x `seq` print x"
       (execToOutWithIn (Seq (Read "x") (Print (Var "x"))) [42])
       (Just [Num 42])
  test "read x `seq` read y `seq` print x `seq` print y"
       (execToOutWithIn ((Read "x") `Seq` (Read "y") `Seq` (Print (Var "x")) `Seq` (Print (Var "y"))) [1, 2])
       (Just [Num 1, Num 2])
  test "read x `seq` read x print x"
       (execToOutWithIn ((Read "x") `Seq` (Read "x") `Seq` (Print (Var "x"))) [1, 2])
       (Just [Num 2])
  -- | NewArray, Set, Get, and ForEach
  test "Array with 5 elements"
       (execToOut (NewArray "array" (num 5) (num 5) `Seq`
                   Set "array" (num 2) (num 42) `Seq`
                   Print (Get "array" (num 0)) `Seq`
                   Print (Get "array" (num 1)) `Seq`
                   Print (Get "array" (num 2)) `Seq`
                   Print (Get "array" (num 3)) `Seq`
                   Print (Get "array" (num 4))))
       (Just [Num 5, Num 5, Num 42, Num 5, Num 5])
  test "foreach print 0 1 2 3"
       (execToOut (NewArray "array" (num 4) (num 0) `Seq`
                   Set "array" (num 1) (num 1) `Seq`
                   Set "array" (num 2) (num 2) `Seq`
                   Set "array" (num 3) (num 3) `Seq`
                   ForEach "x" "array" (Print (Var "x"))))
       (Just [Num 0, Num 1, Num 2, Num 3])
  test "array = [0, 0, 0, 0] `seq` foreach x in array print x `seq` print array"
       (execToOut (NewArray "array" (num 4) (num 0) `Seq`
                   ForEach "x" "array" (Print (Var "x")) `Seq`
                   Print (Var "array")))
       (Just [Num 0, Num 0, Num 0, Num 0, Array [Num 0, Num 0, Num 0, Num 0]])
  -- | exercise 6
  test "exercise 6 where in = [1, 2, 3, 4, 5, 2]"
       (execToOutWithIn exercise6 [1, 2, 3, 4, 5, 2])
       (Just [Num 2, Num 4, Num 6, Num 8, Num 10])
  test "exercise 6 where in = [1, 2, 3, 4, 5, 0]"
       (execToOutWithIn exercise6 [1, 2, 3, 4, 5, 0])
       (Just [Num 0, Num 0, Num 0, Num 0, Num 0])
  test "exercise 6 where in = [1, 0, 1, 0, 1, 5]"
       (execToOutWithIn exercise6 [1, 0, 1, 0, 1, 5])
       (Just [Num 5, Num 0, Num 5, Num 0, Num 5])
  test "exercise 6 where in = [1, 0, 1, 0, 1, -5]"
       (execToOutWithIn exercise6 [1, 0, 1, 0, 1, (-5)])
       (Just [Num (-5), Num 0, Num (-5), Num 0, Num (-5)])
  test "exercise 6 where in = [1, 0, 1, 0, 1, -5, 2]" -- test extra input stream values
       (execToOutWithIn exercise6 [1, 0, 1, 0, 1, (-5), 2])
       (Just [Num (-5), Num 0, Num (-5), Num 0, Num (-5)])
  test "exercise 6 where in = [1, 2, 3, 4, 5]" -- test not enough input stream values
       (execToOutWithIn exercise6 [1, 2, 3, 4, 5])
       Nothing
  -- | exercise 7
  test "exercise 7 where in = [1, 2, 3, 4, 5, 0]"
       (execToOutWithIn exercise7 [1, 2, 3, 4, 5, 0])
       (Just [Num 1, Num 3, Num 6, Num 10, Num 15])
  test "exercise 7 where in = [1, 2, 3, 4, 5, -1]"
       (execToOutWithIn exercise7 [1, 2, 3, 4, 5, -1])
       (Just [Num 1, Num 3, Num 6, Num 10, Num 15])
  test "exercise 7 where in = [10, 20, 30, 40, 50, 60, 70, 0]"
       (execToOutWithIn exercise7 [10, 20, 30, 40, 50, 60, 70, 0])
       (Just [Num 10, Num 30, Num 60, Num 100, Num 150, Num 210, Num 280])
  test "exercise 7 where in = [0]"
       (execToOutWithIn exercise7 [0])
       (Just [])
  test "exercise 7 where in = [-1]"
       (execToOutWithIn exercise7 [-1])
       (Just [])
  test "exercise 7 where in = [1, 2, 3, 4, 5]" -- test there is no number from the input stream that is <= 0
       (execToOutWithIn exercise7 [1, 2, 3, 4, 5])
       Nothing
