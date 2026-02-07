module Ersatz.Counter.Common
  ( parseCounter
  ) where

import Control.Monad.IO.Class
import Ersatz.Solution

parseCounter :: String -> CountResult
parseCounter txt =
    case [n | ('s':' ':rest) <- lines txt
            , Just n <- [parseMc rest]] of
      (n:_) -> Count n
      []    -> Unsolved
  where
    parseMc :: String -> Maybe Integer
    parseMc line =
      case words line of
        ("mc":num:_) -> Just (read num)
        _            -> Nothing
