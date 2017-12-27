module Test where

import Main
import Exp.Lex
import Exp.Par
import Exp.Print
import Exp.Abs
import Exp.Layout
import Exp.ErrM

import System.IO
import Control.Monad
import Data.Char(ord,chr)
import System.Environment
import qualified Data.Map.Strict as Map
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except

import AbstractSyntax
import TypeChecker
import PrettyPrinting
import RawSyntax

someCtx =
  Map.fromList [("I0",(Lam "X" U (Lam "x" (Var "X" 0) (Lam "y" (Var "X" 1) (Lam "f" (Lam "x'" (Var "x" 1) (Var "y" 1)) (Var "x" 2)))),Name "int")),("I1",(Lam "X" U (Lam "x" (Var "X" 0) (Lam "y" (Var "X" 1) (Lam "f" (Lam "x'" (Var "x" 1) (Var "y" 1)) (Var "y" 1)))),Name "int")),("int",(Lam "X" U (Lam "x" (Var "X" 0) (Lam "y" (Var "X" 1) (Lam "f" (Lam "x'" (Var "x" 1) (Var "y" 1)) (Var "X" 3)))),U))]

{-
goalTerm = Lam "i0" (Name "I0") (Lam "A" U (Lam "a" (Var "A" 0) (Lam "y" (Var "A" 1) (Lam "f" (Lam "x'" (Var "a" 1) (Var "y" 1)) ((((Var "i0" 4 :% Var "A" 3) :% Var "a" 2) :% Var "y" 1) :% Var "f" 0)))))
goalType = Lam "i0" (Name "I0") (Lam "X" U (Lam "x" (Var "X" 0) (Lam "y" (Var "X" 1) (Lam "f" (Lam "x'" (Var "x" 1) (Var "y" 1)) (Var "x" 2)))))
-}

-- goalTerm = Lam "B" U (Lam "b" (Var "B" 0) (Lam "b'" (Var "b" 0) (Var "b'" 0)))
-- goalType = Lam "C" U (Lam "c" (Var "C" 0) (Lam "c'" (Var "c" 0) (Var "c" 1)))

goalTerm = Var "b'" 0
goalType = Var "c" 1

testPr :: Proof ()
testPr = do
  local ([Var "c'" 0, Var "c" 0,Var "C" 0,U] ++) (do
    check goalTerm goalType)

--  check goalTerm goalType
--  local (U:) $ local (U:) $ local (Var "A" 0:) $ local (Var "A" 1:) $ local (Lam "x'" (Var "a" 1) (Var "y" 1):) (do
--    infer goalTerm >> return ())

--  check goalTerm goalType

--  local (Name "I0":) $ local (U:) $ local (Var "A" 0:) $ local (Var "A" 1:) $ local (Lam "x'" (Var "a" 1) (Var "y" 1):) (do
--    infer goalTerm >> return ())


-- (Lam "X" U (Lam "x" (Var "X") (Lam "y" (Var "X") (Lam "f" (Lam "x'" (Var "x") (Var "y")) (Var "x")))),Name "int")




