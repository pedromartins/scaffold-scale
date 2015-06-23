{-# LANGUAGE PackageImports, ViewPatterns #-}
module Main where

import Scale.Backends.Legacy as Backends.Legacy
import Scale.Parser as Parser
import Scale.Drivers.Fake as Drivers.Fake

import Scale.Compile
import Scale.Types

import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Maybe
import Data.List
import Text.Parsec.Prim(parse)
import Control.Applicative
import Control.Arrow
import Control.Monad
import System.Environment
import System.IO
import Data.Maybe
import Text.ParserCombinators.Parsec

import Language.Haskell.TH
import Language.Haskell.TH.Ppr

frontends :: [Frontend]
frontends = [Parser.scaleExpr]

backends :: [Backend]
backends = [Backends.Legacy.compileProgram]

drivers :: [Driver]
drivers = [Drivers.Fake.query]

compileString :: String -> Either ParseError [(Program, DepReq)]
compileString s = case parse Parser.scaleExpr "test" s of
                    Left err -> Left err
                    Right e -> Right $ compileExpr e

main :: IO ()
main = do
  [fname] <- getArgs
  s <- readFile fname
  case compileString s of
    Right (unzip -> (ps, qs)) ->
      forM (zip [1..length ps] ps) $ \(i,p) -> do
        doc <- Backends.Legacy.compileProgram $ p
        B.writeFile (fname ++ show i) doc
    Left e -> (print e) >> fail "parse error"
  return ()

