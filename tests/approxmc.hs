{-# LANGUAGE ScopedTypeVariables #-}
import Prelude hiding ((||), not)
import Ersatz
import Ersatz.Bit
import Ersatz.Variable (exists)
import Ersatz.Counter.ApproxMC (approxmc)

import System.Environment (getArgs)
import Control.Monad ( forM_, replicateM, unless )
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStr, hClose)
import System.Process (readProcessWithExitCode)
import Ersatz.Solution (CountResult(..))
import Ersatz.Counter.Common (parseCounter)
import System.Exit (exitFailure, exitSuccess)

data TestCase = TestCase
  { testName :: String
  , cnfContent :: String
  , expectedCount :: Integer
  }

testCases :: [TestCase]
testCases =
  [ TestCase
      { testName = "test-0-v=15-c=5"
      , cnfContent = unlines
          [ "p cnf 15 5"
          , "1 2 0"
          , "3 4 0"
          , "11 4 5 0"
          , "-14 11 0"
          , "1 2 3 4 5 6 7 8 -9 10 11 0"
          , "c p show 1 2 3 4 5 6 7 8 9 10 0"
          ]
      , expectedCount = 576
      }
    , TestCase
      { testName = "test-1-v=15-c=5"
      , cnfContent = unlines
          [ "p cnf 15 5"
          , "1 2 0"
          , "3 4 0"
          , "11 4 5 0"
          , "-14 11 0"
          , "1 2 3 4 5 6 7 8 -9 10 11 0"
          , "c p show 1 2 3 4 5 0"
          ]
      , expectedCount = 18
      }
  , TestCase
      { testName = "test-2-v=2-c=1"
      , cnfContent = unlines
          [ "p cnf 2 1"
          , "1 2 0"
          , "c p show 1 2 0"
          ]
      , expectedCount = 3
      }
  ]

runTest :: TestCase -> IO Bool
runTest (TestCase testName cnfContent expectedCount) = do
  putStrLn $ "\n=== Running: " ++ testName ++ " ==="
  
  withSystemTempFile "test.cnf" $ \path handle -> do
    hPutStr handle cnfContent
    hClose handle
    
    putStrLn $ "Invoking approxmc on: " ++ path
    (_exit, out, err) <- readProcessWithExitCode "approxmc" [path] []
    
    unless (null err) $ do
      putStrLn "=== Stderr ==="
      putStrLn err
    
    let result = parseCounter out
    
    case result of
      Count n -> do
        if n == expectedCount
          then do
            putStrLn $ "✓ PASSED: Got expected count of " ++ show expectedCount
            return True
          else do
            putStrLn $ "✗ FAILED: Expected " ++ show expectedCount ++ ", got " ++ show n
            putStrLn "=== ApproxMC Output ==="
            putStrLn out
            return False
      CountUnsolved -> do
        putStrLn "✗ FAILED: Could not parse count"
        putStrLn "=== ApproxMC Output ==="
        putStrLn out
        return False

approxmcInterfaceTest :: IO ()
approxmcInterfaceTest = do
  
  (_, problem) <- runSAT $ do
    (x1 :: Bit) <- exists
    (x2 :: Bit) <- exists
    (x3 :: Bit) <- exists
    (x4 :: Bit) <- exists
    (x5 :: Bit) <- exists
    (x6 :: Bit) <- exists
    (x7 :: Bit) <- exists
    (x8 :: Bit) <- exists
    (x9 :: Bit) <- exists
    (x10 :: Bit) <- exists
    (x11 :: Bit) <- exists
    (x12 :: Bit) <- exists
    (x13 :: Bit) <- exists
    (x14 :: Bit) <- exists
    (x15 :: Bit) <- exists
    
    assert (x1 || x2)
    assert (x3 || x4)
    assert (x11 || x4 || x5)
    assert ((not x14) || x11)
    assert (x1 || x2 || x3 || x4 || x5 || x6 || x7 || x8 || (not x9) || x10 || x11)
    return ()
  
  result <- approxmc problem [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  case result of
    Count n -> 
      putStrLn $ "Approximate count: " ++ show n ++ " solutions"
    CountUnsolved -> 
      putStrLn "Could not count solutions"
    _ -> 
      putStrLn $ "Unexpected result: " ++ show result

main :: IO ()
main = do
  results <- mapM runTest testCases
  
  let passed = length (filter id results)
      total = length results
  
  putStrLn "\n====================================="
  putStrLn $ "Results: " ++ show passed ++ "/" ++ show total ++ " tests passed"
  putStrLn "====================================="
  approxmcInterfaceTest
  
  if passed == total
    then exitSuccess
    else exitFailure