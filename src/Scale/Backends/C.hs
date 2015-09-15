{-# LANGUAGE QuasiQuotes #-}
module C where

import Scale.Types

compileProgram :: Backend
compileProgram (p,q) = undefined
  where
  compileProgram' :: Program -> C
  compileProgram' (PVar x) = [c| x |]
  compileProgram' (PApp p p') = [c| $(compileProgram' p)($(compileProgram' p')) |]
  compileProgram' (PIf p p' p'') = [c| if($(compileProgram' p))
                                       {$(compileProgram' p')}
                                       else{$(compileProgram' p'')} |]
  compileProgram' (Pub d) = [c| broadcast($(stringE d)) |]
  compileProgram' (SubC c) = [c| while(true){querySource($(stringE c))}; |]
  compileProgram' (Sub d) = [c| r = querySource(d)); readings.modify($(stringE d),r) |]
  compileProgram' (Read d) = [c| readings.read(d) |]
  compileProgram' (PCmd c) = [c| executeCommand(c) |]
  compileProgram' (PWith r p) = compileProgram' p
  compileProgram' (PConstr i) = [c| $i |]
  compileProgram' (Seq p p') = [c| $(compileProgram' p); $(compileProgram' p') |]
