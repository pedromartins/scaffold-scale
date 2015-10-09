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

exprToProgram :: Expr -> Program
exprToProgram (Var i) = PVar i
exprToProgram (Lam i e) = PLam i (exprToProgram e)
exprToProgram (App e e') = PApp (exprToProgram e) (exprToProgram e')
exprToProgram (If e e' e'') = PIf (exprToProgram e)
                                  (exprToProgram e')
                                  (exprToProgram e'')
exprToProgram (DataQ d) = Read d
exprToProgram (DataBracket d e) = Sub d `Seq` (exprToProgram e)
exprToProgram (Cmd c) = PCmd c
exprToProgram (With r e) = PWith r (exprToProgram e)
exprToProgram (Constr i) = PConstr i

compileExpr e =
  let dataQs = collectData e
      commands = collectCommands e
  in    map (\d -> (Pub d, Provides d)) dataQs
     ++ map (\c -> (SubC c, IsCapableOf c)) commands
     ++ [ (foldl Seq (exprToProgram e) . map Sub $ dataQs, Any) ]

