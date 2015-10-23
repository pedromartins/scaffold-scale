{-# LANGUAGE Rank2Types #-}
module Scale.Frontends.GScale.Parser where

import Control.Applicative

import Data.Char
import Text.Parsec.String
import Text.Parsec.Prim(try)
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Scale.Types
import Scale.Frontends.GScale.Types

scaleDef = haskellStyle
  { reservedOpNames = [ "?", "¢", ".", "→", "=", ":", ">" ]
  , reservedNames = [ "with", "ccase", "of", "Case", "if", "then", "else"]
  }

scaleLexer = Token.makeTokenParser scaleDef
identifier = Token.identifier scaleLexer
dataScale = identifier
command = identifier
requirement = identifier
reserved = Token.reserved scaleLexer
operator = Token.operator scaleLexer
reservedOp = Token.reservedOp scaleLexer
stringLiteral = Token.stringLiteral scaleLexer
whiteSpace = Token.whiteSpace scaleLexer
parens = Token.parens scaleLexer
semiSep = Token.semiSep scaleLexer
braces = Token.braces scaleLexer
brackets = Token.brackets scaleLexer
symbol = Token.symbol scaleLexer

scaleExpr :: (forall x. Parser x -> Parser (f x)) -> Parser (Scale f)
scaleExpr plf = foldl1 App <$> sepBy (scaleExpr' plf) whiteSpace
  where
  scaleExpr' :: (forall x. Parser x -> Parser (f x)) -> Parser (Scale f)
  scaleExpr' plf =
        DataQ <$> (reservedOp "?" *> dataScale)
    <|> try (DataBracket <$> dataScale <*> brackets (scaleExpr plf))
    <|> Cmd <$> (reserved "cmd" *> command)
    <|> With <$> (reserved "with" *> requirement) <*> sepBy identifier (reservedOp ",") <*> (whiteSpace *> (scaleExpr plf))
    <|> parens (scaleExpr plf)
    <|> SubLang <$> (plf (scaleExpr plf))

scaleDecl :: (forall x. Parser x -> Parser (f x)) -> Parser (Decl f)
scaleDecl plf = try (ValAssgn <$> identifier <*> (reservedOp "=" *> (scaleExpr plf)))

builtins = [ ]

