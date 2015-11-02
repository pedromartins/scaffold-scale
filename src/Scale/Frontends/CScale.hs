module Scale.Frontends.CScale where

import Control.Applicative
import Data.Char
import qualified Data.ByteString.Char8 as B
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.Parsec.Prim(try)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Language

import Scale.Types
import Scale.Frontends.CScale.Types
import Scale.Frontends.CScale.Compiler
import Scale.Frontends.CScale.Parser

cScale :: Frontend
cScale s = case Parsec.parse scaleExpr "test" (B.unpack s) of
                    Left err -> Left err
                    Right e -> Right $ compileExpr e

