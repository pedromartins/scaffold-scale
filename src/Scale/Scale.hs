{-# LANGUAGE PackageImports, ViewPatterns #-}
module Main where

import Scale.Backends.Legacy as Backends.Legacy
import Scale.Frontends.FScale as Frontends.FScale

import Scale.Types

import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Maybe
import Data.List
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
frontends = [("fscale", Frontends.FScale.fScale)]

frontendReader :: ReadM Frontend
frontendReader =
  eitherReader (\s -> case lookup s frontends of
                        Just f -> Right f
                        Nothing -> Left "Invalid frontend.")

backends :: [(String, Backend)]
backends = [("legacy", Backends.Legacy.compileProgram)]

backendReader :: ReadM Backend
backendReader =
  eitherReader (\s -> case lookup s backends of
                        Just f -> Right f
                        Nothing -> Left "Invalid backend.")
data CompilerArgs =
  CompilerArgs { frontend :: Frontend
               , backend :: Backend
               , iFile :: String
               }

defaultFrontend = Frontends.FScale.fScale
defaultBackend = Backends.Legacy.compileProgram

readMaybe :: Read a => ReadM (Maybe a)
readMaybe = eitherReader $ \arg -> case reads arg of
  [(r, "")] -> return (Just r)
  _         -> return Nothing

parseScaleArgs = CompilerArgs <$>
      option frontendReader (value defaultFrontend <> short 'f' <> long "frontend"
                              <> help ("Frontend to use. Frontends available:" ++ (show (map fst frontends))))
  <*> option backendReader (value defaultBackend <> short 'b' <> long "backend"
                              <> help ("Backend to use. Backends available:" ++ (show (map fst backends))))
  <*> strArgument (help "Input file")

main :: IO ()
main = do
  CompilerArgs frontend backend fname <- execParser (info (helper <*> parseScaleArgs) fullDesc)
  s <- B.readFile fname
  case frontend s of
    Right (unzip -> (ps, qs)) ->
      forM (zip3 [1..length ps] ps qs) $ \(i,p,q) -> do
        doc <- backend $ (p,q)
        B.writeFile (fname ++ show i) doc
    Left e -> (print e) >> fail "parse error"
  return ()

