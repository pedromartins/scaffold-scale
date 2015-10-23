module Scale.Frontends.FScale.Compiler where

import Data.Data

import Data.Generics
import Data.Generics.Schemes

import Scale.Types
import Scale.Frontends.FScale.Types

compileModule :: (Module, Requirement) -> [(Program, DepReq)]
compileModule = concatMap compileDecl . fst

compileDecl :: Decl -> [(Program, DepReq)]
compileDecl (ValAssgn i e) =
  (compileExpr e)

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

exprToProgram q e = let (p, pqs) = exprToProgram' q e in (p,q):pqs
  where
    exprToProgram' :: DepReq -> Expr -> (Program, [(Program, DepReq)])
    exprToProgram' q (Var i) = (PVar i, [])
    exprToProgram' q (Lam i e) = (PLam i p, pqs)
      where (p, pqs) = exprToProgram' q e
    exprToProgram' q (App e1 e2) = (PApp p1 p2, pqs1 ++ pqs2)
      where (p1, pqs1) = exprToProgram' q e1
            (p2, pqs2) = exprToProgram' q e2
    exprToProgram' q (If e1 e2 e3) = (PIf p1 p2 p3, pqs1 ++ pqs2 ++ pqs3)
      where (p1, pqs1) = exprToProgram' q e1
            (p2, pqs2) = exprToProgram' q e2
            (p3, pqs3) = exprToProgram' q e3
    exprToProgram' q (DataQ d) = (Read d, [])
    exprToProgram' q (DataBracket d e) = (Sub (DataMessage d) `Seq` p, pqs)
      where (p, pqs) = exprToProgram' q e
    exprToProgram' q (Cmd c) = (Pub (CommandMessage c), [])
    exprToProgram' q (With r vs e) = (Pub (HeapMessage vs) `Seq` (Sub (HeapMessage vs)),
      pqs ++ [(Sub (HeapMessage vs) `Seq` p `Seq` (Pub (HeapMessage vs)), Fulfills r)])
      where (p, pqs) = exprToProgram' (q `And` Fulfills r) e
    exprToProgram' q (Constr i) = (PConstr i, [])

compileExpr e =
  let dataQs = collectData e
      commands = collectCommands e
  in    map (\d -> (Pub (DataMessage d), Provides d)) dataQs
     ++ map (\c -> (Sub (CommandMessage c), IsCapableOf c)) commands
     ++ exprToProgram Any e

