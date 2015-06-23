
module Scale.Parser where

import Control.Applicative

import Data.Char
import Text.Parsec.String
import Text.Parsec.Prim(try)
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Scale.Types

scaleDef = haskellStyle
  { reservedOpNames = [ "λ",  "?", "¢", ".", "→", "=", ":" ]
  , reservedNames = [ "with", "ccase", "of", "Case", "if", "then", "else"]
  }

scaleLexer = Token.makeTokenParser scaleDef
identifier = Token.identifier scaleLexer
dataExpr = identifier
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

scaleStmt :: Parser Stmt
scaleStmt  =  try (Bind <$> identifier <*> (reservedOp "<-" *> scaleExpr))
          <|> ExprStmt <$> scaleExpr

scaleExpr :: Parser Expr
scaleExpr = foldl1 App <$> sepBy scaleExpr' whiteSpace
  where
    scaleExpr' :: Parser Expr
    scaleExpr' =  Lam <$> (reservedOp "λ" *> identifier) <*> (reservedOp "." *> scaleExpr)
              <|> If <$> (reserved "if" *> scaleExpr)
                     <*> (reserved "then" *> scaleExpr)
                     <*> (reserved "else" *> scaleExpr)
              <|> DataQ <$> (reservedOp "?" *> dataExpr)
              <|> try (DataBracket <$> dataExpr <*> brackets scaleExpr)
              <|> Cmd <$> (reservedOp "¢" *> command)
              <|> With <$> (reserved "with" *> requirement) <*> (whiteSpace *> scaleExpr)
              <|> parens scaleExpr
              <|> (do x <- identifier
                      case lookup x builtins of
                        Just e -> return e
                        Nothing -> return $ Var x)

scaleDecl :: Parser Decl
scaleDecl =  try (ValAssgn <$> identifier <*> (reservedOp "=" *> scaleExpr))

builtins = [ ]

