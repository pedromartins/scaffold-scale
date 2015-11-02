{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module Scale.Frontends.GScale.Compiler where

import Data.Data

import Data.Generics
import Data.Generics.Schemes

import Scale.Types
import Scale.Frontends.GScale.Types
import Control.Monad.State.Lazy

type NodeCompiler x = x -> Program
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

exprToProgram :: forall f. String -> (forall x. Compiler x -> Compiler (f x))
              -> DepReq -> (Scale f) -> [(Program, DepReq)]
exprToProgram a clf q e = let (p, pqs) = evalState (exprToProgram' q e) [1..] in (p,q):pqs
  where
    exprToProgram' :: DepReq -> (Scale f) -> State [Integer] (Program, [(Program, DepReq)])
    exprToProgram' q (DataQ d) = return (Read d, [])
    exprToProgram' q (DataBracket d e) = do
      (p, pqs) <- exprToProgram' q e
      return (Sub (DataMessage d) `Seq` p, pqs)
    exprToProgram' q (Cmd c) = return (Pub (CommandMessage c), [])
    exprToProgram' q (With r vs e) = do
      (p, pqs) <- exprToProgram' (q `And` Fulfills r) e
      (n:ns) <- get
      put ns
      return (Pub (HeapMessage a n vs) `Seq` (Sub (ResultMessage a n p)),
        pqs ++ [(Sub (HeapMessage a n vs) `Seq` (Pub (ResultMessage a n p)), Fulfills r)])
    exprToProgram' q (SubLang e) = do
      ns <- get
      let exprToProgram'' q e = evalState (exprToProgram' q e) ns
      return (clf exprToProgram'' q e)
    exprToProgram' q (App s1 s2) = do
      (p1, pqs1) <- exprToProgram' q s1
      (p2, pqs2) <- exprToProgram' q s2
      return (PApp p1 p2, pqs1 ++ pqs2)

compileScale :: String
             -> (forall a. a -> ((Scale f) -> a) -> f (Scale f) -> a)
             -> (forall a. Compiler a -> Compiler (f a))
             -> (Scale f)
             -> [(Program, DepReq)]
compileScale a rec clf e =
  let dataQs = collectData rec e
      commands = collectCommands rec e
  in    map (\d -> (Pub (DataMessage d), Provides d)) dataQs
     ++ map (\c -> (Sub (CommandMessage c), IsCapableOf c)) commands
     ++ exprToProgram a clf Any e

