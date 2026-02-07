{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Ersatz
import Control.Monad (replicateM)
import Prelude hiding ((||), (&&), not)

-- 4-bit addition problem
additionExample :: IO ()
additionExample = do
  putStrLn "=== Addition Example ==="
  putStrLn "Finding two 4-bit numbers that add to 23..."
  
  (result, mbSolution) <- solveWith minisat $ do
    a <- Bits <$> replicateM 4 exists
    b <- Bits <$> replicateM 4 exists
    
    let sum' = a + b
    
    assert (sum' === encode (23 :: Integer))
    
    return (a, b)
  
  case (result, mbSolution) of
    (Satisfied, Just (aVal, bVal)) -> 
      putStrLn $ "Found: " ++ show (aVal :: Integer) ++ " + " ++ show (bVal :: Integer) ++ " = " ++ show (aVal + bVal)
    _ -> putStrLn "No solution found"

-- 2-SAT
booleanExample :: IO ()
booleanExample = do
  putStrLn "\n=== Boolean SAT Example ==="
  putStrLn "Solving: (a OR b) AND (NOT a OR c) AND (NOT b OR NOT c)"
  
  (result, mbSolution) <- solveWith minisat $ do
    a <- exists
    b <- exists
    c <- exists
    
    assert ((a || b) && (not a || c) && (not b || not c))
    
    return (a, b, c)
  
  case (result, mbSolution) of
    (Satisfied, Just (a, b, c)) -> 
      putStrLn $ "Solution: a=" ++ show (a :: Bool) ++ ", b=" ++ show (b :: Bool) ++ ", c=" ++ show (c :: Bool)
    _ -> putStrLn "No solution found"

main :: IO ()
main = do
  putStrLn "Ersatz SAT Solver Playground\n"
  additionExample
  booleanExample
  putStrLn "\nDone!"