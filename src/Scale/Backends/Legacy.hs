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
import qualified Scale.Backends.Legacy.Prelude as P
import Scale.Backends.Legacy.Prelude hiding ((>>=),(==),(.),return,lookup,readIORef)

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Ppr

$(deriveLift ''Program)

-- Pseudo code with TH notation
compileProgram :: Backend
compileProgram (p,q) = do
  prog <- runQ . fmap (B.append (B.pack "module Main where\n\
                                               \import Control.Monad\n\
                                               \import Data.Maybe\n\
                                               \import Data.IORef\n\
                                               \import Scale.Backends.Legacy.Prelude\n\
                                               \main = do\n\
                                               \  readings <- newIORef []\n\
                                               \  ")
                             . B.pack . show . ppr) . compileProgram' $ p
  let depreq = B.pack $ "-- " ++ (show q) ++ "\n"
  return $ B.append depreq prog
  where
    compileProgram' :: Program -> ExpQ
    compileProgram' (PVar x) = varE . mkName $ x
    compileProgram' (PApp p p') = [| $(compileProgram' p) $(compileProgram' p') |]
    compileProgram' (PIf p p' p'') = [| $(compileProgram' p) P.>>= \cond ->
      if cond P.== "1" then $(compileProgram' p') else $(compileProgram' p'') |]
    compileProgram' (Pub d) = [| broadcast $(stringE d) |]
    compileProgram' (SubC c) = [| forever ((querySource $(stringE c)) P.>>= (\_ -> P.return unit)) |]
    compileProgram' (Sub d) = [| querySource $(stringE d) P.>>=
      (\r -> modifyIORef $(varE . mkName $ "readings") (P.modify $(stringE d) r)) |]
    compileProgram' (Read d) = [| (P.readIORef $(varE . mkName $ "readings")) P.>>=
      (P.return P.. fromJust P.. P.lookup $(stringE d)) |]
    compileProgram' (PCmd c) = [| executeCommand $(stringE c) |]
    compileProgram' (PWith r p) = [| $(compileProgram' p) |]
    compileProgram' (PConstr i) = [| i |]
    compileProgram' (Seq p p') = [| $(compileProgram' p) P.>> $(compileProgram' p') |]

