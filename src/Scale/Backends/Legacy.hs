{-# LANGUAGE DataKinds, KindSignatures, RankNTypes, MultiParamTypeClasses,
             FlexibleInstances, TypeFamilies, FlexibleContexts, ScopedTypeVariables,
             GADTs, GeneralizedNewtypeDeriving, EmptyDataDecls, TemplateHaskell #-}
module Scale.Backends.Legacy where

import Data.Maybe
import Data.IORef
import Scale.Drivers.Fake
import qualified Data.ByteString.Char8 as B
import System.Process
import Control.Concurrent.MVar
import Control.Monad
import System.IO
import Scale.Types

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Ppr

modify :: (Eq a) => a -> b -> [(a,b)] -> [(a,b)]
modify k v [] = [(k,v)]
modify k v (kv@(k',_):kvs')
  | k == k' = (k,v) : kvs'
  | otherwise = kv : modify k v kvs'

$(deriveLift ''Program)

unit = ()

-- Pseudo code with TH notation
compileProgram :: Backend
compileProgram = runQ . fmap (B.append (B.pack "module Main where\n\
                                               \import Control.Concurrent.MVar\n\
                                               \import Control.Monad\n\
                                               \import Data.Maybe\n\
                                               \import Data.IORef\n\
                                               \import GHC.IORef\n\
                                               \import GHC.MVar\n\
                                               \import GHC.Base\n\
                                               \import GHC.Classes\n\
                                               \import GHC.Tuple\n\
                                               \import GHC.List\n\
                                               \import Scale.Backends.Legacy\n\
                                               \main = do\n\
                                               \  readings <- newIORef []\n\
                                               \  ")
                             . B.pack . show . ppr) . compileProgram'
  where
    compileProgram' :: Program -> ExpQ
    compileProgram' (PVar x) = varE . mkName $ x
    compileProgram' (PApp p p') = [| $(compileProgram' p) $(compileProgram' p') |]
    compileProgram' (PIf p p' p'') = [| $(compileProgram' p) >>= \cond ->
      if cond == "1" then $(compileProgram' p') else $(compileProgram' p'') |]
    compileProgram' (Pub d) = [| broadcast $(stringE d) |]
    compileProgram' (SubC c) = [| forever $ querySource $(stringE c) >>= (\_ -> return unit) |]
    compileProgram' (Sub d) = [| querySource $(stringE d) >>=
      (\r -> modifyIORef $(varE . mkName $ "readings") (modify $(stringE d) r)) |]
    compileProgram' (Read d) = [| readIORef $(varE . mkName $ "readings") >>=
      return . fromJust . lookup $(stringE d) |]
    compileProgram' (PCmd c) = [| executeCommand $(stringE c) |]
    compileProgram' (PWith r p) = [| $(compileProgram' p) |]
    compileProgram' (PConstr i) = [| i |]
    compileProgram' (Seq p p') = [| $(compileProgram' p) >> $(compileProgram' p') |]

topic s = "/" ++ s

mosquittoTopic = "test"

loadSpec = undefined

broadcast :: DataQuery -> IO Integer
broadcast q = forever $ do
 qv <- query q
 (_, _, _, ph) <- runInteractiveCommand $ "mosquitto_pub  -t " ++ mosquittoTopic ++ (topic q) ++ " -m " ++ qv
 putStrLn $ mosquittoTopic ++ topic q ++ " -> " ++ (show $ qv)
 waitForProcess ph
 return qv

subscribe :: String -> (String -> IO Bool) -> IO ()
subscribe q hdl = do
  -- FIXME: The following code assumes one reading per line
  (_, oh, _, ph) <- runInteractiveCommand $ "mosquitto_sub -t " ++ mosquittoTopic ++ (topic q)
  putStrLn $ mosquittoTopic ++ topic q ++ "?"
  ls <- fmap lines . hGetContents $ oh
  -- ls :: [String]
  process $ ls
  terminateProcess ph
  return ()
  where
    process (l:ls) = do
      cont <- hdl l
      if cont then process ls else return ()
    process [] = return ()

querySource q = do
  c <- newIORef ""
  subscribe q (\r -> writeIORef c r >> return False)
  readIORef c >>= \cv -> putStrLn $ mosquittoTopic ++ topic q ++ " <- " ++ cv
  readIORef c

handleQuery :: DataQuery -> (String -> IO Bool) -> IO ()
handleQuery = subscribe

executeCommand :: Command -> IO ()
executeCommand c = putStrLn (c ++ "!") >> broadcast c >> return ()

