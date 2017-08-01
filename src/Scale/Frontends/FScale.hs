-- Copyright 2017 Pedro M. N. Martins, Julie A. McCann, Imperial College London 
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
-- 1. Redistributions of source code must retain the above copyright notice, this
-- list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above copyright notice,
-- this list of conditions and the following disclaimer in the documentation
-- and/or other materials provided with the distribution.
-- 
-- 3. Neither the name of the copyright holder nor the names of its contributors
-- may be used to endorse or promote products derived from this software without
-- specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

module Scale.Frontends.FScale where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Either
import Data.List
import qualified Data.ByteString.UTF8 as B8
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.Parsec.Prim(try)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Error
import Text.Parsec.Pos

import Scaffold.Types
import Scale.Frontends.FScale.Types
import Scale.Frontends.FScale.Compiler
import Scale.Frontends.FScale.Parser

fScale :: Frontend
fScale flags s =
  case mapM (Parsec.parse scaleDecl "") . map unlines . splitBlocks . lines $ (B8.toString s) of
    Left err -> fail (show err)
    Right d -> do
      when (elem "dump-parse" flags) $ print d
      return (compileModule "test" d)

splitBlocks :: [String] -> [[String]]
splitBlocks = splitBlocks' . filter (not . null)
  where
    splitBlocks' [] = []
    splitBlocks' (l:ls) =
      case findIndex (\l -> not . isSpace $ head l) ls of
        Just ix -> let (b1,bs) = splitAt ix ls
                   in (l:b1):(splitBlocks' bs)
        Nothing -> [l:ls]
