{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Ersatz
import Ersatz.Counter.ApproxMC (approxmc)
import Control.Monad (replicateM)
import Prelude hiding ((||), (&&), not)

-- 4-bit addition problem
additionExample :: Integer -> IO ()
additionExample target = do
  putStrLn "=== Addition Example ==="
  putStrLn $ "Finding two 4-bit numbers that add to " ++ show target ++ "..."
  
  (result, mbSolution) <- solveWith minisat $ do
    a <- Bits <$> replicateM 4 exists
    b <- Bits <$> replicateM 4 exists
    
    let sum' = a + b
    
    assert (sum' === encode target)
    
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

-- ApproxMC example 1
approxmcCountingExample :: IO ()
approxmcCountingExample = do
  putStrLn "\n=== ApproxMC Example 1 ==="
  putStrLn "Counting solutions for: (a OR b) AND (c OR d)"
  putStrLn "Projection variables: [1, 2] (counting over a and b)"
  
  (_, problem) <- runSAT $ do
    a <- exists
    b <- exists
    c <- exists
    d <- exists
    
    assert (a || b)
    assert (c || d)
    
    return ()
  
  result <- approxmc problem [1, 2]
  
  case result of
    Count n -> 
      putStrLn $ "Approximate count: " ++ show n ++ " solutions"
    CountUnsolved -> 
      putStrLn "Could not count solutions"
    _ -> 
      putStrLn $ "Unexpected result: " ++ show result

-- ApproxMC example 2
approxmcAllSolutionsExample :: IO ()
approxmcAllSolutionsExample = do
  putStrLn "\n=== ApproxMC Example 2 ==="
  putStrLn "Counting all solutions for: (a OR b) AND (NOT a OR c) AND (NOT b OR NOT c)"
  
  (_, problem) <- runSAT $ do
    a <- exists
    b <- exists
    c <- exists
    
    assert ((a || b) && (not a || c) && (not b || not c))
    
    return ()
  
  result <- approxmc problem [1, 2, 3]
  
  case result of
    Count n -> 
      putStrLn $ "Approximate count: " ++ show n ++ " total solutions"
    CountUnsolved -> 
      putStrLn "Could not count solutions"
    _ -> 
      putStrLn $ "Unexpected result: " ++ show result


main :: IO ()
main = do
  putStrLn "Ersatz SAT Solver Playground\n"
  additionExample 23
  additionExample 42
  booleanExample
  approxmcCountingExample
  approxmcAllSolutionsExample
  putStrLn "\nDone!"