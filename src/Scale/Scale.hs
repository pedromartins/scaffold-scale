{-# LANGUAGE PackageImports, ViewPatterns #-}
module Main where

import Scale.Backends.Legacy as Backends.Legacy
import Scale.Backends.LegacyZMQ as Backends.LegacyZMQ
import Scale.Frontends.FScale as Frontends.FScale
import Scale.Frontends.GScale.GFScale as Frontends.GFScale

import Scale.Types

import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split
import Text.Parsec.Prim(parse)
import Control.Applicative
import Control.Arrow
import Control.Monad
import Options.Applicative
import Options.Applicative.Builder
import System.Environment
import System.IO
import System.Exit
import Data.Maybe
import qualified Text.ParserCombinators.Parsec as Parsec

import Language.Haskell.TH
import Language.Haskell.TH.Ppr

frontends :: [(String, Frontend)]
frontends = [("fscale", Frontends.FScale.fScale)
            ,("gfscale", Frontends.GFScale.gfScale)]

frontendReader :: ReadM Frontend
frontendReader =
  eitherReader (\s -> case lookup s frontends of
                        Just f -> Right f
                        Nothing -> Left "Invalid frontend.")

backends :: [(String, Backend)]
backends = [("legacy", Backends.Legacy.compileProgram)
           ,("legacyzmq", Backends.LegacyZMQ.compileProgram)]

backendReader :: ReadM Backend
backendReader =
  eitherReader (\s -> case lookup s backends of
                        Just f -> Right f
                        Nothing -> Left "Invalid backend.")
data CompilerArgs =
  CompilerArgs { frontend :: Frontend
               , backend :: Backend
               , opts :: [String]
               , iFile :: String
               }

defaultFrontend = Frontends.FScale.fScale
defaultBackend = Backends.Legacy.compileProgram

readMaybe :: Read a => ReadM (Maybe a)
readMaybe = eitherReader $ \arg -> case reads arg of
  [(r, "")] -> return (Just r)
  _         -> return Nothing

parseScaleArgs = CompilerArgs <$>
      option frontendReader (value defaultFrontend <> short 'f' <> long "frontend" <> metavar "<frontend>"
                              <> help ("Frontend to use. Frontends available:" ++ (unwords (map fst frontends))))
  <*> option backendReader (value defaultBackend <> short 'b' <> long "backend" <> metavar "<backend>"
                              <> help ("Backend to use. Backends available:" ++ (unwords (map fst backends))))
  <*> fmap (splitOn ",") (strOption (value "" <> long "opts" <> metavar "<opts>" <> help "Comma separated list of options to pass to frontend/backend."))
  <*> strArgument (metavar "file")

main :: IO ()
main = do
  CompilerArgs frontend backend opts fname <- execParser (info (helper <*> parseScaleArgs) fullDesc)
  s <- B.readFile fname
  (ps, qs) <- unzip <$> frontend opts s
  forM (zip3 [1..length ps] ps qs) $ \(i,p,q) -> do
    doc <- backend opts qs $ (p,q)
    B.writeFile (fname ++ show i) doc
  return ()

