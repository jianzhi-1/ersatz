import Ersatz
import Ersatz.Bit
import Ersatz.Variable (exists)

import System.Environment (getArgs)
import Control.Monad ( forM_, replicateM, unless )
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStr, hClose)
import System.Process (readProcessWithExitCode)
import Ersatz.Solution (CountResult(..))
import Ersatz.Counter.Common (parseCounter)


main :: IO ()
main = do
  let cnfContent = unlines
        [ "p cnf 15 5"
        , "1 2 0"
        , "3 4 0"
        , "11 4 5 0"
        , "-14 11 0"
        , "1 2 3 4 5 6 7 8 -9 10 11 0"
        , "c p show 1 2 3 4 5 0"
        ]
  
  putStrLn "Writing CNF to temp file..."
  withSystemTempFile "test.cnf" $ \path handle -> do
    hPutStr handle cnfContent
    hClose handle
    
    putStrLn $ "Invoking approxmc on: " ++ path
    (_exit, out, err) <- readProcessWithExitCode "approxmc" [path] []
    
    putStrLn "=== ApproxMC Output ==="
    putStrLn out
    
    unless (null err) $ do
      putStrLn "=== Stderr ==="
      putStrLn err
    
    putStrLn "=== Parsing Result ==="
    let result = parseCounter out
    print result
    
    case result of
      Count n -> 
        if n == 18
          then putStrLn "✓ Test PASSED: Got expected count of 18"
          else putStrLn $ "✗ Test FAILED: Expected 18, got " ++ show n
      CountUnsolved -> 
        putStrLn "✗ Test FAILED: Could not parse count"
      _ -> 
        putStrLn $ "✗ Test FAILED: Unexpected result: " ++ show result

