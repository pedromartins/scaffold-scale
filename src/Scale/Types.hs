{-# LANGUAGE DeriveDataTypeable #-}
module Scale.Types where

import Data.ByteString
import Data.Data

type DataQuery = String
type Command = String
type Ident = String
type Requirement = String

data Stmt = ExprStmt Expr
          | Bind Ident Expr
          deriving (Eq, Show)

data Expr = Var Ident
          | Lam Ident Expr
          | App Expr Expr
          | If Expr Expr Expr
          | DataQ DataQuery
          | DataBracket DataQuery Expr
          | Cmd Command
          | With Requirement Expr
          | Constr Ident
          deriving (Data, Typeable, Eq, Show)

data Decl = ValAssgn Ident Expr
          deriving (Eq, Show)

data DepReq = Provides DataQuery
            | IsCapableOf Command
            | Fulfills Requirement
            | Any
            deriving (Data, Typeable, Eq, Show, Read)

type Module = [Decl]

data Program = PVar Ident
          | PLam Ident Program
          | PApp Program Program
          | PIf Program Program Program
          | Pub DataQuery
          | SubC Command
          | Sub DataQuery
          | Read DataQuery
          | PCmd Command
          | PWith Requirement Program
          | PConstr Ident
          | Seq Program Program
          deriving (Eq, Show)

-- TODO:Should be a newtype: ByteString is not specific enough
type Backend = (Program, DepReq) -> IO ByteString

-- TODO:Should be a newtype: String is not specific enough
type Driver = String

