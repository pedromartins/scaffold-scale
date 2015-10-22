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
import Control.Monad.Fix
import System.IO
import Scale.Types
import qualified Scale.Backends.Legacy.Prelude as P
import Scale.Backends.Legacy.Prelude hiding ((>>=),(==),(.),return,lookup,readIORef)

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Ppr

$(deriveLift ''Message)
$(deriveLift ''Program)

-- Pseudo code with TH notation
compileProgram :: Backend
compileProgram flags (p,q) = do
  when (elem "dump-prog" flags) (print (p,q))
  -- TODO: get arg for drivers?
  prog <- runQ . fmap (B.append (B.pack "module Main where\n\
                                               \import Control.Monad\n\
                                               \import Data.Maybe\n\
                                               \import Data.IORef\n\
                                               \import System.Environment\n\
                                               \import Scale.Types\n\
                                               \import Scale.Backends.Legacy.Prelude\n\
                                               \main = do\n\
                                               \  drivers <- (fmap (read Prelude.. head) getArgs :: IO [(DepReq, Driver)])\n\
                                               \  readings <- newIORef []\n\
                                               \  ")
                             . B.pack . show . ppr) . compileProgram' $ p
  let depreq = B.pack $ "-- " ++ (show q) ++ "\n"
  return $ B.append depreq prog
  where
    compileProgram' :: Program -> ExpQ
    compileProgram' (PVar "forever") = varE . mkName $ "Control.Monad.forever"
    compileProgram' (PVar x) = varE . mkName $ x
    -- compileProgram' (PLam i e) = lamE [varP . mkName $ i] (compileProgram e)
    compileProgram' (PApp p p') = [| $(compileProgram' p) $(compileProgram' p') |]
    compileProgram' (PIf p p' p'') = [| $(compileProgram' p) P.>>= \cond ->
      if cond then $(compileProgram' p') else $(compileProgram' p'') |]
    compileProgram' (PWhile p p') = [| fix (\f -> $(compileProgram' p) P.>>= \cond -> if cond then $(compileProgram' p') P.>>= f else return ()) |]
    compileProgram' (Pub (DataMessage d)) = [| forever (broadcast $(varE . mkName $ "drivers") $(stringE d)) |]
    compileProgram' (Pub (CommandMessage c)) = [| broadcastSimple $(stringE c) |]
    compileProgram' (Sub (DataMessage d)) = [| querySource $(stringE d) P.>>=
      (\r -> modifyIORef $(varE . mkName $ "readings") (P.modify $(stringE d) r)) |]
    compileProgram' (Sub (CommandMessage c)) = [| forever ((querySource $(stringE c)) P.>>=
      (\_ -> P.executeCommand $(varE . mkName $ "drivers") $(stringE c))) |]
    compileProgram' (Read d) = [| (P.readIORef $(varE . mkName $ "readings")) P.>>=
      (P.return P.. fromJust P.. P.lookup $(stringE d)) |]
    compileProgram' (PWith r p) = [| $(compileProgram' p) |]
    compileProgram' (PConstr i) = [| i |]
    compileProgram' (Seq p p') = [| $(compileProgram' p) P.>> $(compileProgram' p') |]

