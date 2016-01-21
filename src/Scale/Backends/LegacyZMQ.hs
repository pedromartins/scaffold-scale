{-# LANGUAGE DataKinds, KindSignatures, RankNTypes, MultiParamTypeClasses,
             FlexibleInstances, TypeFamilies, FlexibleContexts, ScopedTypeVariables,
             GADTs, GeneralizedNewtypeDeriving, EmptyDataDecls, TemplateHaskell #-}
module Scale.Backends.LegacyZMQ where

import Control.Arrow
import Data.Maybe
import Data.IORef
import Scaffold.Drivers.POSIX
import qualified Data.ByteString.Char8 as B
import System.Process
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Fix
import System.IO
import Scaffold.Types
import qualified Scale.Backends.LegacyZMQ.Prelude as P
import Scale.Backends.LegacyZMQ.Prelude hiding ((>>=),(==),(.),return,lookup,readIORef)

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Ppr

$(deriveLift ''Message)
$(deriveLift ''Program)

-- Pseudo code with TH notation
compileProgram :: Backend
compileProgram flags _ (p,q) = do
  when (elem "dump-prog" flags) (print (p,q))
  -- TODO: get arg for drivers?
  prog <- runQ . fmap (B.append (B.pack ("module Main where\n\
                                               \import System.ZMQ4 as ZMQ\n\
                                               \import Control.Monad\n\
                                               \import Control.Arrow\n\
                                               \import Data.Maybe\n\
                                               \import Data.IORef\n\
                                               \import System.Environment\n\
                                               \import Scale.Types\n\
                                               \import Scale.Drivers.POSIX\n\
                                               \import Scale.Backends.LegacyZMQ.Prelude\n\
                                               \main = ZMQ.withContext $ \\c -> do\n\
                                               \  args <- getArgs\n\
                                               \  let drivers :: [(DepReq, Driver)]\n\
                                               \      nodes :: [(DepReq, Node)]\n\
                                               \      (drivers,nodes) = case args of\n\
                                               \                          [] -> ([], [])\n\
                                               \                          [sdrivers] -> (Prelude.read sdrivers, [])\n\
                                               \                          [sdrivers,snodes] -> (Prelude.read sdrivers, Prelude.read snodes)\n\
                                               \  readings <- newIORef []\n\
                                               \  let none = \"_|_\"\n"
                                               Prelude.++ initSockets
                                               Prelude.++ "\n  "))
                             . B.pack . P.show . ppr) . compileProgram' $ p
  let depreq = B.pack $ "-- " P.++ (P.show q) P.++ "\n"
  return $ B.append depreq prog
  where
    initSockets =
      (          if (not . null . filter isProvides $ qs) then "  publisher <- ZMQ.socket c ZMQ.Pub\n\
                                                               \  ZMQ.bind publisher \"tcp://*:5556\"\n" else ""
      Prelude.++ if (not . null . filter isCapable $ qs)  then "  subscriber <- ZMQ.socket c ZMQ.Sub\n" else "")
      where qs = depreqToList q

    isProvides (Provides _) = True
    isProvides _ = False

    isCapable (IsCapableOf _) = True
    isCapable _ = False

    depreqToList (qs1 `And` qs2) = depreqToList qs1 Prelude.++ depreqToList qs2
    depreqToList q = [q]

    compileProgram' :: Program -> ExpQ
    compileProgram' (PVar "forever") = varE . mkName $ "Control.Monad.forever"
    compileProgram' (PVar x) = varE . mkName $ x
    -- compileProgram' (PLam i e) = lamE [varP . mkName $ i] (compileProgram e)

    compileProgram' (PApp p p') = [| $(compileProgram' p) $(compileProgram' p') |]

    compileProgram' (PIf p p' p'') = [| $(compileProgram' p) P.>>= \cond ->
      if cond then $(compileProgram' p') else $(compileProgram' p'') |]

    compileProgram' (PWhile p p') = [| fix (\f -> $(compileProgram' p) P.>>= \cond -> if cond then $(compileProgram' p') P.>>= f else return ()) |]

    compileProgram' (Pub (DataMessage d)) = [| forever (query ($(conE . mkName $ "Provides") $(stringE d)) $(varE . mkName $ "drivers") P.>>= pub $(varE . mkName $ "publisher") d) |]
    compileProgram' (Pub (CommandMessage c)) = [| pub $(varE . mkName $ "publisher") $(stringE "") $(stringE "!") |]
    compileProgram' (Pub (HeapMessage a n vs)) = [| pub (a P.++ "/" P.++ P.show n) (P.show $(listE . map (\v -> (tupE [stringE v, varE . mkName $ v])) $ vs)) |]
    compileProgram' (Pub (ResultMessage a n p)) = [| $(compileProgram' p) P.>>= pub (a P.++ "/" P.++ P.show n P.++ "/result") P.>> P.ret P.unit |]

    compileProgram' (Sub (DataMessage d)) = [| sub (P.fromJust (P.lookup ($(conE . mkName $ "Provides") $(stringE d)) $(varE . mkName $ "nodes"))) $(stringE d) P.>>=
      (\r -> modifyIORef $(varE . mkName $ "readings") (P.modify $(stringE d) r)) |]
    compileProgram' (Sub (CommandMessage c)) = [| forever (pull $(stringE c) P.>>=
      (\_ -> P.executeCommand $(varE . mkName $ "drivers") $(stringE c))) |]
    compileProgram' (Seq (Sub (HeapMessage a n vs)) p) = do
      vvs <- newName "vvs"
      infixE (Just [| sub (a P.++ "/" P.++ P.show n) |]) [|(P.>>=)|] (Just $ lamE [varP vvs]
        (letE (   (map (\v -> valD (varP . mkName $ v) (normalB (appE [| P.fromJust |] (appE [| P.lookup v P.. P.read |] (varE vvs)))) []) vs)
              P.++(map (\v -> sigD (mkName v) (conT . mkName $ "String")) vs))
          (compileProgram' p)))
    compileProgram' (Sub (ResultMessage a n p)) = [| sub (a P.++ "/" P.++ P.show n P.++ "/result") |]

    compileProgram' (Read d) = [| (P.readIORef $(varE . mkName $ "readings")) P.>>=
      (P.return P.. P.fromJust P.. P.lookup $(stringE d)) |]
    compileProgram' (PWith r p) = [| $(compileProgram' p) |]
    compileProgram' (PConstr i) = [| i |]
    compileProgram' (Seq p p') = [| $(compileProgram' p) P.>> $(compileProgram' p') |]
    compileProgram' (PExecve s) = [| execve $(stringE s) |]

