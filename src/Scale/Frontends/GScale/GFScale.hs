{-# LANGUAGE StandaloneDeriving, Rank2Types, DeriveFunctor, FlexibleInstances #-}
module Scale.Frontends.GScale.GFScale where

import Control.Monad
import Data.Char
import Control.Applicative
import Text.Parsec.String
import Text.Parsec.Prim(try)
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Data.ByteString.Char8 as B

import Scale.Types
import Scale.Frontends.GScale.Types
import Scale.Frontends.GScale.Parser
import Scale.Frontends.GScale.Compiler

import Text.Parsec.String

data LamF a = LamE String a | VarE String
  deriving (Functor, Show)

deriving instance (Show (Scale LamF))

lamFScale :: Parser x -> Parser (LamF x)
lamFScale px =
      LamE <$> (reservedOp "\\" *> identifier) <*> (reservedOp "." *> px)
  <|> VarE <$> identifier

compileLamF :: forall a. Compiler a -> Compiler (LamF a)
compileLamF cx q (LamE i e) = (PLam i p, pqs)
  where (p, pqs) = cx q e
compileLamF cx q (VarE i) = (PVar i, [])

recLamF :: a -> ((Scale LamF) -> a) -> LamF (Scale LamF) -> a
recLamF unit phi (LamE _ e) = phi e
recLamF unit phi (VarE _) = unit

compileFScale = compileScale "test" recLamF compileLamF

gfScale :: Frontend
gfScale flags s =
  case Parsec.parse (scaleDecl lamFScale) "test" (B.unpack s) of
    Left err -> fail (show err)
    Right (ValAssgn _ e) -> do
      when (elem "dump-parse" flags) $ print e
      return (compileScale "test" recLamF compileLamF e)
