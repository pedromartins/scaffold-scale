{-# LANGUAGE DeriveDataTypeable #-}
module Scale.Frontends.FScale.Types where

import Data.ByteString
import Data.Data
import qualified Text.ParserCombinators.Parsec as Parsec

import Scaffold.Types

data Stmt = ExprStmt Expr
          | Bind Ident Expr
          deriving (Eq, Show)

data Expr = Var Ident
          | Op Ident
          | Lam Ident Expr
          | App Expr Expr
          | If Expr Expr Expr
          | DataQ DataQuery
          | DataBracket DataQuery Expr
          | Cmd Command
          | With Requirement [Ident] Expr
          | Constr Ident
          | IntLit Integer
          | StringLit String
          | Let [(Ident, Expr)] Expr
          deriving (Data, Typeable, Eq, Show)

data Decl = ValAssgn Ident Expr
          | Import Ident
          deriving (Eq, Show)

type Module = [Decl]
