{-# LANGUAGE ScopedTypeVariables #-}
module Ersatz.Counter.ApproxMC 
  ( approxmc
  , approxmcPath
  ) where

import Control.Monad.IO.Class
import Ersatz.Problem ( SAT, writeDimacs', writeProjectionSet )
import Ersatz.Solution

-- | `Counter` for `SAT` problems that tries to invoke the @approxmc@ executable from the @PATH@
approxmc :: MonadIO m => [Int] -> Counter SAT m CountResult
approxmc projectionVars = approxmcPath "approxmc" projectionVars

-- | `Counter` for `SAT` problems that tries to invoke a program that takes @approxmc@ compatible arguments.
--
-- The 'FilePath' refers to the path to the executable.
approxmcPath :: MonadIO m => FilePath -> [Int] -> Counter SAT m
approxmcPath path pv problem = liftIO $ do
  withTempFiles ".cnf" "" $ \problemPath _ -> do
    writeProjectionSet pv
    writeDimacs' problemPath problem
    (_exit, out, _err) <-
      readProcessWithExitCode path ["-dimacs", problemPath] []
    
    let result = case lines out of
                    "s SATISFIABLE":_ -> Satisfied
                    "s UNSATISFIABLE":_ -> Unsatisfied
                    _ -> Unsolved
  return (result, parseSolution out)

parseSolution :: B.ByteString -> IntMap Bool
-- TODO
-- parseSolution s =
--   case B.words s of
--     x : ys | x == "SAT" ->
--           List.foldl'
--                  ( \ m y -> case B.readInt y of
--                               Just (v,_) -> if 0 == v then m else IntMap.insert (abs v) (v>0) m
--                               Nothing    -> error $ "parseSolution: Expected an Int, received " ++ show y
--                  ) IntMap.empty ys
--     _ -> IntMap.empty -- WRONG (should be Nothing)