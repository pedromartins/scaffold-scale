module Scale.Frontends.FScale.Compiler where

import Data.Data

import Data.Generics
import Data.Generics.Schemes

import Scale.Types
import Scale.Frontends.FScale.Types
import Control.Monad.State.Lazy

compileModule :: String -> (Module, Requirement) -> [(Program, DepReq)]
compileModule a = concatMap (compileDecl a) . fst

compileDecl :: String -> Decl -> [(Program, DepReq)]
compileDecl a (ValAssgn i e) =
  (compileExpr a e)

collectData :: Expr -> [DataQuery]
collectData = everything (++) (mkQ [] extractData)
  where extractData (DataQ d) = [d]
        extractData _ = []

collectCommands :: Expr -> [Command]
collectCommands = everything (++) (mkQ [] extractCommands)
  where extractCommands (Cmd c) = [c]
        extractCommands _ = []

-- Without 'with r e', it suffices to have:
-- exprToProgram :: Expr -> Program
-- exprToProgram (Var i) = PVar i
-- exprToProgram (Lam i e) = PLam i (exprToProgram e)
-- exprToProgram (App e e') = PApp (exprToProgram e) (exprToProgram e')
-- exprToProgram (If e e' e'') = PIf (exprToProgram e)
--                                   (exprToProgram e')
--                                   (exprToProgram e'')
-- exprToProgram (DataQ d) = Read d
-- exprToProgram (DataBracket d e) = Sub d `Seq` (exprToProgram e)
-- exprToProgram (Cmd c) = PCmd c
-- exprToProgram (With r e) = PWith r (exprToProgram e)
-- exprToProgram (Constr i) = PConstr i
--
-- as there is only one computation element (p, Any).

exprToProgram a q e = let (p, pqs) = evalState (exprToProgram' q e) [1..] in (p,q):pqs
  where
    exprToProgram' :: DepReq -> Expr -> State [Integer] (Program, [(Program, DepReq)])
    exprToProgram' q (Var i) = return (PVar i, [])
    exprToProgram' q (Lam i e) = do
      (p, pqs) <- exprToProgram' q e
      return (PLam i p, pqs)
    exprToProgram' q (App e1 e2) = do
      (p1, pqs1) <- exprToProgram' q e1
      (p2, pqs2) <- exprToProgram' q e2
      return (PApp p1 p2, pqs1 ++ pqs2)
    exprToProgram' q (If e1 e2 e3) = do
      (p1, pqs1) <- exprToProgram' q e1
      (p2, pqs2) <- exprToProgram' q e2
      (p3, pqs3) <- exprToProgram' q e3
      return (PIf p1 p2 p3, pqs1 ++ pqs2 ++ pqs3)
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
    exprToProgram' q (Constr i) = return (PConstr i, [])

compileExpr a e =
  let dataQs = collectData e
      commands = collectCommands e
  in    map (\d -> (Pub (DataMessage d), Provides d)) dataQs
     ++ map (\c -> (Sub (CommandMessage c), IsCapableOf c)) commands
     ++ exprToProgram a Any e

