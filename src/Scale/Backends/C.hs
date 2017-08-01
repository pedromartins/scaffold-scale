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
