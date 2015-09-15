module Scale.Drivers.POSIX where

import Data.Maybe
import System.Process
import System.IO

import Scale.Types

query :: DepReq -> [(DepReq, Driver)] -> IO String
query q drivers = do
  (_, oh, _, ph) <- runInteractiveCommand (fromJust $ lookup q drivers)
  l <- hGetLine oh
  return l

