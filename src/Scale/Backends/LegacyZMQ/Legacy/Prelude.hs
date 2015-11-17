module Scale.Backends.Legacy.Prelude where

import Control.Concurrent
import Data.IORef
import Data.Maybe
import Scale.Drivers.POSIX
import qualified Data.ByteString.Char8 as B
import System.Process
import Control.Monad
import System.IO
import Scale.Types

modify :: (Eq a) => a -> b -> [(a,b)] -> [(a,b)]
modify k v [] = [(k,v)]
modify k v (kv@(k',_):kvs')
  | k Prelude.== k' = (k,v) : kvs'
  | otherwise = kv : modify k v kvs'

broker = "localhost"
mosquittoTopic = "test"
topic s = "/" Prelude.++ s
loadSpec = undefined

-- TODO: Reliable pub.
pub :: String -> String -> IO ()
pub t m = do
 threadDelay 1000000
 putStrLn $ "mosquitto_pub  -h " Prelude.++ broker Prelude.++ " -t " Prelude.++ topic mosquittoTopic Prelude.++ topic t Prelude.++ " -m '" Prelude.++ m Prelude.++ "'"
 (_, _, _, ph) <- runInteractiveCommand $ "mosquitto_pub  -h " Prelude.++ broker Prelude.++ " -t " Prelude.++ topic mosquittoTopic Prelude.++ topic t Prelude.++ " -m '" Prelude.++ m Prelude.++ "'"
 threadDelay 1000000
 waitForProcess ph
 Prelude.return ()

-- TODO: Timeout sub with default action
sub :: String -> IO String
sub t = do
  -- FIXME: The following code assumes one reading per line
  putStrLn $ "mosquitto_sub -h " Prelude.++ broker Prelude.++ " -t " Prelude.++ topic mosquittoTopic Prelude.++ topic t
  (_, oh, _, ph) <- runInteractiveCommand $ "mosquitto_sub -h " Prelude.++ broker Prelude.++ " -t " Prelude.++ topic mosquittoTopic Prelude.++ topic t
  l <- hGetLine $ oh
  terminateProcess ph
  Prelude.return l

executeCommand :: [(DepReq, Driver)] -> Command -> IO ()
executeCommand drivers c = do
  (_, _, _, ph) <- runInteractiveCommand (Data.Maybe.fromJust $ Prelude.lookup (IsCapableOf c) drivers)
  waitForProcess ph
  Prelude.return ()

(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
(>>=) = (Prelude.>>=)

(>>) :: (Monad m) => m a -> m b -> m b
(>>) = (Prelude.>>)

(==) :: (Eq a) => a -> a -> Bool
(==) = (Prelude.==)

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = (Prelude..)

(++) :: [a] -> [a] -> [a]
(++) = (Prelude.++)

return :: (Monad m) => a -> m a
return = Prelude.return

unit :: ()
unit = ()

lookup :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup = Prelude.lookup

readIORef = Data.IORef.readIORef

ret :: (Monad m) => a -> m a
ret = Prelude.return

threshold :: IO String -> IO Bool
threshold mi = mi Prelude.>>= (Prelude.return Prelude.. (>500) Prelude.. Prelude.read)

execve = undefined

show :: (Show a) => a -> String
show = Prelude.show

concat :: [[a]] -> [a]
concat = Prelude.concat

fromJust :: Maybe a -> a
fromJust = Data.Maybe.fromJust

read :: (Read a) => String -> a
read = Prelude.read

bind :: (Monad m) => m a -> (a -> m b) -> m b
bind = (Prelude.>>=)
