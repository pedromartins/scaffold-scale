module Scale.Frontends.GScale where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Either
import qualified Data.ByteString.Char8 as B
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.Parsec.Prim(try)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Error
import Text.Parsec.Pos

import Scale.Types
import Scale.Frontends.GScale.Types
import Scale.Frontends.GScale.Compiler
import Scale.Frontends.GScale.Parser

gScale :: Frontend
gScale flags s =
  case Parsec.parse scaleDecl "test" (B.unpack s) of
    Left err -> fail (show err)
    Right d -> do
      when (elem "dump-parse" flags) $ print d
      return (compileDecl d)

