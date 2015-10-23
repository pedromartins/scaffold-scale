{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module Scale.Frontends.GScale.Compiler where

import Data.Data

import Data.Generics
import Data.Generics.Schemes

import Scale.Types
import Scale.Frontends.GScale.Types

type Compiler x = DepReq -> x -> (Program, [(Program, DepReq)])

collectData :: (forall a. a -> ((Scale f) -> a) -> f (Scale f) -> a) -> (Scale f) -> [DataQuery]
collectData rec (DataQ d) = [d]
collectData rec (DataBracket _ e) = collectData rec e
collectData rec (Cmd _) = []
collectData rec (With _ _ e) = collectData rec e
collectData rec (SubLang sl) = rec [] (collectData rec) sl
collectData rec (App e e') = collectData rec e ++ collectData rec e'

collectCommands :: (forall a. a -> ((Scale f) -> a) -> f (Scale f) -> a) -> (Scale f) -> [DataQuery]
collectCommands rec (DataQ _) = []
collectCommands rec (DataBracket _ e) = collectCommands rec e
collectCommands rec (Cmd c) = [c]
collectCommands rec (With _ _ e) = collectCommands rec e
collectCommands rec (SubLang sl) = rec [] (collectCommands rec) sl
collectCommands rec (App e e') = collectCommands rec e ++ collectCommands rec e'

exprToProgram :: forall f. (forall x. Compiler x -> Compiler (f x))
              -> DepReq -> (Scale f) -> [(Program, DepReq)]
exprToProgram clf q e = let (p, pqs) = exprToProgram' q e in (p,q):pqs
  where
    exprToProgram' :: DepReq -> (Scale f) -> (Program, [(Program, DepReq)])
    exprToProgram' q (DataQ d) = (Read d, [])
    exprToProgram' q (DataBracket d e) = (Sub (DataMessage d) `Seq` p, pqs)
      where (p, pqs) = exprToProgram' q e
    exprToProgram' q (Cmd c) = (Pub (CommandMessage c), [])
    exprToProgram' q (With r vs e) = (Pub (HeapMessage vs) `Seq` (Sub (HeapMessage vs)),
      pqs ++ [(Sub (HeapMessage vs) `Seq` p `Seq` (Pub (HeapMessage vs)), Fulfills r)])
      where (p, pqs) = exprToProgram' (q `And` Fulfills r) e
    exprToProgram' q (SubLang e) = clf exprToProgram' q e

compileScale :: (forall a. a -> ((Scale f) -> a) -> f (Scale f) -> a)
             -> (forall a. Compiler a -> Compiler (f a))
             -> (Scale f)
             -> [(Program, DepReq)]
compileScale rec clf e =
  let dataQs = collectData rec e
      commands = collectCommands rec e
  in    map (\d -> (Pub (DataMessage d), Provides d)) dataQs
     ++ map (\c -> (Sub (CommandMessage c), IsCapableOf c)) commands
     ++ exprToProgram clf Any e

