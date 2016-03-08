module Scale.Frontends.FScale where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Either
import Data.List
import qualified Data.ByteString.UTF8 as B8
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.Parsec.Prim(try)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Error
import Text.Parsec.Pos

import Scaffold.Types
import Scale.Frontends.FScale.Types
import Scale.Frontends.FScale.Compiler
import Scale.Frontends.FScale.Parser

fScale :: Frontend
fScale flags s =
  case mapM (Parsec.parse scaleDecl "") . map unlines . splitBlocks . lines $ (B8.toString s) of
    Left err -> fail (show err)
    Right d -> do
      when (elem "dump-parse" flags) $ print d
      return (compileModule "test" d)

splitBlocks :: [String] -> [[String]]
splitBlocks = splitBlocks' . filter (not . null)
  where
    splitBlocks' [] = []
    splitBlocks' (l:ls) =
      case findIndex (\l -> not . isSpace $ head l) ls of
        Just ix -> let (b1,bs) = splitAt ix ls
                   in (l:b1):(splitBlocks' bs)
        Nothing -> [l:ls]
