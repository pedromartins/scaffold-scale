{-# LANGUAGE DeriveDataTypeable #-}
module Scale.Types where

import Data.ByteString
import Data.Data
import qualified Text.ParserCombinators.Parsec as Parsec

type DataQuery = String
type Command = String
type Ident = String
type Requirement = String
type Node = String

data DepReq = Provides DataQuery
            | IsCapableOf Command
            | Fulfills Requirement
            | And DepReq DepReq
            | Any
            deriving (Data, Typeable, Eq, Show, Read)

data Message = DataMessage DataQuery
             | CommandMessage Command
             | HeapMessage String Integer [Ident]
             | ResultMessage String Integer Program
             deriving (Eq, Show)

data Program = PVar Ident
             | PLam Ident Program
             | PApp Program Program
             | PIf Program Program Program
             | PWhile Program Program
             | Pub Message
             | Sub Message
             | Read DataQuery
             | PWith Requirement Program
             | PConstr Ident
             | Seq Program Program
             | PExecve String
             deriving (Eq, Show)

-- TODO:Should be a newtype: ByteString is not specific enough
type Option = String

type Frontend = [Option] -> ByteString -> IO [(Program, DepReq)]
type Backend = [Option] -> (Program, DepReq) -> IO ByteString

-- TODO:Should be a newtype: String is not specific enough
type Driver = String

