import Ersatz.Counter.Common (parseCounter)
import Ersatz.Solution (CountResult(..))
import System.Exit (exitFailure, exitSuccess)

testCases :: [(String, CountResult, String)]
testCases =
  [ ("s mc 576\n", Count 576, "basic case")
  , ("s mc 0\n", Count 0, "zero count")
  , ("s mc 1234567890\n", Count 1234567890, "large number")
  , ("c comment\ns mc 42\n", Count 42, "with comment line")
  , ("s mc 100\ns mc 200\n", Count 100, "multiple mc lines (takes first)")
  , ("unrelated\ns mc 99\nmore stuff\n", Count 99, "with surrounding text")
  , ("s SATISFIABLE\n", CountUnsolved, "no mc line")
  , ("", CountUnsolved, "empty input")
  , ("mc 100\n", CountUnsolved, "mc without 's' prefix")
  , ("s other 100\n", CountUnsolved, "s line without mc")
  , ("s mc\n", CountUnsolved, "mc without number")
  , ("s mc abc\n", CountUnsolved, "mc with non-numeric value")
  ]

runTest :: (String, CountResult, String) -> IO Bool
runTest (input, expected, description) = do
  let result = parseCounter input
  if result == expected
    then do
      putStrLn $ "✓ " ++ description
      return True
    else do
      putStrLn $ "✗ " ++ description
      putStrLn $ "  Input: " ++ show input
      putStrLn $ "  Expected: " ++ show expected
      putStrLn $ "  Got: " ++ show result
      return False

main :: IO ()
main = do
  putStrLn "Running parseCounter tests..."
  results <- mapM runTest testCases
  let passed = length (filter id results)
      total = length results
  putStrLn $ "\n" ++ show passed ++ "/" ++ show total ++ " tests passed"
  if Prelude.all id results
    then exitSuccess
    else exitFailure
