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
import Control.Monad.Reader hiding (liftIO)
import Control.Monad.State hiding (liftIO)
import Control.Monad.Except hiding (liftIO)
import Control.Monad.Trans.Except hiding (liftIO)

import AbstractSyntax
import TypeChecker
import PrettyPrinting
import RawSyntax


goalType =
    (Lam Plus "a'" (Var "a" 2) (Var "b" 2))
goalTerm =
    (Var "f" 0)

-- check (Var "a'" 0) Minus (Var "a" 3)

testPr :: Proof ()
testPr = do
  local ([(U 0, Plus), (U 0, Plus)] ++ ) $
    check (Lam Iso "a'" (Var "a" 1) (Var "b" 1)) Plus (Lam Plus "a'" (Var "a" 1) (U 0))

--             

--  check goalTerm goalType
--  local (U:) $ local (U:) $ local (Var "A" 0:) $ local (Var "A" 1:) $ local (Lam "x'" (Var "a" 1) (Var "y" 1):) (do
--    infer goalTerm >> return ())

--  check goalTerm goalType

--  local (Name "I0":) $ local (U:) $ local (Var "A" 0:) $ local (Var "A" 1:) $ local (Lam "x'" (Var "a" 1) (Var "y" 1):) (do
--    infer goalTerm >> return ())


-- (Lam "X" U (Lam "x" (Var "X") (Lam "y" (Var "X") (Lam "f" (Lam "x'" (Var "x") (Var "y")) (Var "x")))),Name "int")




