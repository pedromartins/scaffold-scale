{-# LANGUAGE DeriveDataTypeable #-}
module Scale.Types where

import Data.ByteString
import Data.Data
import qualified Text.ParserCombinators.Parsec as Parsec

type DataQuery = String
type Command = String
type Ident = String
type Requirement = String

data DepReq = Provides DataQuery
            | IsCapableOf Command
            | Fulfills Requirement
            | Any
            deriving (Data, Typeable, Eq, Show, Read)

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
type Frontend = ByteString -> Either Parsec.ParseError [(Program, DepReq)]
type Backend = (Program, DepReq) -> IO ByteString

-- TODO:Should be a newtype: String is not specific enough
type Driver = String

