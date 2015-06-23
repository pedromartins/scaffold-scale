module Scale.Backends.Legacy.Prelude where

import Data.IORef
import Data.Maybe
import Scale.Drivers.Fake
import qualified Data.ByteString.Char8 as B
import System.Process
import Control.Concurrent.MVar
import Control.Monad
import System.IO
import Scale.Types

modify :: (Eq a) => a -> b -> [(a,b)] -> [(a,b)]
modify k v [] = [(k,v)]
modify k v (kv@(k',_):kvs')
  | k Prelude.== k' = (k,v) : kvs'
  | otherwise = kv : modify k v kvs'

mosquittoTopic = "test"
topic s = "/" ++ s
loadSpec = undefined

broadcast :: DataQuery -> IO Integer
broadcast q = forever $ do
 qv <- query q
 (_, _, _, ph) <- runInteractiveCommand $ "mosquitto_pub  -t " ++ mosquittoTopic ++ (topic q) ++ " -m " ++ qv
 putStrLn $ mosquittoTopic ++ topic q ++ " -> " ++ (show $ qv)
 waitForProcess ph
 Prelude.return qv

-- FIXME: REWRITE!
subscribe :: String -> IO String
subscribe q = do
  -- FIXME: The following code assumes one reading per line
  (_, oh, _, ph) <- runInteractiveCommand $ "mosquitto_sub -t " ++ mosquittoTopic ++ (topic q)
  putStrLn $ mosquittoTopic ++ topic q ++ "?"
  l <- hGetLine $ oh
  terminateProcess ph
  Prelude.return l
  -- ls :: [String]

querySource :: String -> IO String
querySource = subscribe

executeCommand :: Command -> IO ()
executeCommand c = putStrLn (c ++ "!") Prelude.>> broadcast c Prelude.>> Prelude.return ()

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
