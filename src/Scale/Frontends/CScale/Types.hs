{-# LANGUAGE DeriveDataTypeable #-}
module Scale.Frontends.CScale.Types where

import Data.ByteString
import Data.Data
import qualified Text.ParserCombinators.Parsec as Parsec

import Scale.Types

data Expr = Var Ident
          | If Expr Expr Expr
          | While Expr Expr
          | DataQ DataQuery
          | DataBracket DataQuery Expr
          | Cmd Command
          | With Requirement Expr
          deriving (Data, Typeable, Eq, Show)

data Decl = ValAssgn Ident Expr
          deriving (Eq, Show)

type Module = [Decl]
