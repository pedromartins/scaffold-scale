{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleContexts#-}
module Scale.Frontends.GScale.Types where

import Data.ByteString
import Data.Data
import qualified Text.ParserCombinators.Parsec as Parsec

import Scaffold.Types

data Scale f = DataQ DataQuery
            | DataBracket DataQuery (Scale f)
            | Cmd Command
            | With Requirement [Ident] (Scale f)
            | SubLang (f (Scale f))
            | App (Scale f) (Scale f)

data Decl f = ValAssgn Ident (Scale f)

type Module f = [Decl f]

