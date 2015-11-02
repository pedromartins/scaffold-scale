module Scale.Backends.LegacyZMQ.Prelude where

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

broker = "achilles.doc.ic.ac.uk"
mosquittoTopic = "test"
topic s = "/" ++ s
loadSpec = undefined

broadcastSimple :: DataQuery -> IO ()
broadcastSimple q = do
 (_, _, _, ph) <- runInteractiveCommand $ "mosquitto_pub  -h " ++ broker ++ " -t " ++ mosquittoTopic ++ (topic q) ++ " -m " ++ show q
 putStrLn $ mosquittoTopic ++ topic q ++ " -> " ++ (show $ q)
 threadDelay 1000000
 waitForProcess ph
 Prelude.return ()

broadcast :: [(DepReq, Driver)] -> DataQuery -> IO Integer
broadcast drivers q = forever $ do
 qv <- query (Provides q) drivers
 (_, _, _, ph) <- runInteractiveCommand $ "mosquitto_pub  -h " ++ broker ++ " -t " ++ mosquittoTopic ++ (topic q) ++ " -m " ++ qv
 putStrLn $ mosquittoTopic ++ topic q ++ " -> " ++ (show $ qv)
 waitForProcess ph
 Prelude.return qv

-- FIXME: REWRITE!
subscribe :: String -> IO String
subscribe q = do
  -- FIXME: The following code assumes one reading per line
  (_, oh, _, ph) <- runInteractiveCommand $ "mosquitto_sub -h " ++ broker ++ " -t " ++ mosquittoTopic ++ (topic q)
  putStrLn $ mosquittoTopic ++ topic q ++ "?"
  l <- hGetLine $ oh
  terminateProcess ph
  Prelude.return l
  -- ls :: [String]

querySource :: String -> IO String
querySource = subscribe

executeCommand :: [(DepReq, Driver)] -> Command -> IO ()
executeCommand drivers c = do
  (_, _, _, ph) <- runInteractiveCommand (fromJust $ Prelude.lookup (IsCapableOf c) drivers)
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
