module Scale.Frontends.FScale where

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
import Scale.Frontends.FScale.Types
import Scale.Frontends.FScale.Compiler
import Scale.Frontends.FScale.Parser

fScale :: Frontend
fScale flags s =
  case Parsec.parse scaleDecl "test" (B.unpack s) of
    Left err -> fail (show err)
    Right d -> do
      when (elem "dump-parse" flags) $ print d
      return (compileDecl d)

