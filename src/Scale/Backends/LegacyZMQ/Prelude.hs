module Scale.Backends.LegacyZMQ.Prelude where

import System.ZMQ4 as ZMQ
import Control.Concurrent
import Data.IORef
import Data.Maybe
import Scaffold.Drivers.POSIX
import qualified Data.ByteString.Char8 as B
import System.Process
import Control.Monad
import System.IO
import Scaffold.Types

modify :: (Eq a) => a -> b -> [(a,b)] -> [(a,b)]
modify k v [] = [(k,v)]
modify k v (kv@(k',_):kvs')
  | k Prelude.== k' = (k,v) : kvs'
  | otherwise = kv : modify k v kvs'

broker = "localhost"
mosquittoTopic = "test"
topic s = "/" Prelude.++ s
loadSpec = undefined

pub :: ZMQ.Socket Pub -> String -> String -> IO ()
pub s t m = do
  putStrLn $ "PUB " Prelude.++ t Prelude.++ " > " Prelude.++ m
  ZMQ.send s [] (B.pack (Prelude.unwords [t,m]))

sub :: Node -> String -> IO String
sub b t = ZMQ.withContext $ \c -> do
  putStrLn $ "SUB " Prelude.++ t Prelude.++ " @ " Prelude.++ b
  subscriber <- ZMQ.socket c ZMQ.Sub
  ZMQ.connect subscriber ("tcp://" Prelude.++ b Prelude.++ ":5556")
  ZMQ.subscribe subscriber (B.pack t)
  update <- ZMQ.receive subscriber
  let [_, r] = Prelude.words $ B.unpack update
  Prelude.return r

push :: Context -> String -> String -> IO ()
push c b t = do
  putStrLn $ "PUSH " Prelude.++ t Prelude.++ " @ " Prelude.++ b
  sender <- ZMQ.socket c ZMQ.Push
  ZMQ.connect sender ("tcp://" Prelude.++ b Prelude.++ ":5558")
  ZMQ.send sender [] (B.pack "!")

pull :: Context -> String -> IO ()
pull c t = do
  putStrLn $ "PULL " Prelude.++ t
  receiver <- ZMQ.socket c ZMQ.Pull
  ZMQ.bind receiver "tcp://*:5558"
  ZMQ.receive receiver
  Prelude.return ()

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
