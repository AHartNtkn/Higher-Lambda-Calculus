module RawSyntax where

import AbstractSyntax
import PrettyPrinting

import Control.Monad.Except

-- Representation for printing
import Exp.Abs
import Exp.ErrM

import Control.Monad
import Data.String

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM

{- Intermediate raw syntax -}
data ITerm
   = IVS String
   | IV String Int
   | IDec ITerm ITerm
   | ILam String ITerm ITerm
   | IApp ITerm ITerm
   | IU Int
   deriving (Show)

{- Convert intermediate syntax into abstract syntax -}

-- Replace string variables with deBruijin indexes
index :: String -> Int -> ITerm -> ITerm
index s n (IVS x) = if s == x then IV x n else IVS x
index s n (IV st x) = IV st x
index s n (IDec t t1) = IDec (index s n t) (index s n t1)
index s n (ILam x d d1) = ILam x (index s n d) (index s (1 + n) (index x 0 d1))
index s n (IApp d d1) = IApp (index s n d) (index s n d1)
index s n (IU i) = IU i

-- Convert intermediate syntax into abstract syntax
fromInter :: ITerm -> Proof Term
fromInter (IV s x) = return $ Var s x
fromInter (IVS x) = return $ Name x
fromInter (IDec d d1) = Dec <$> fromInter d <*> fromInter d1
fromInter (IApp d d1) = (:%) <$> fromInter d <*> fromInter d1
fromInter (ILam x d d1) = Lam x <$> fromInter d <*> fromInter d1
fromInter (IU i) = return (U i)

{- Convert concrete syntax into intermediate syntax -}
fromCon :: Exp -> Proof ITerm
fromCon (SLet d e) = proofError "TO DO: Implement let expressions"
fromCon (SApp a b) = IApp <$> fromCon a <*> fromCon b
fromCon (SDec a b) = IDec <$> fromCon a <*> fromCon b
fromCon (SVar (AIdent e)) = return $ IVS e
fromCon (SLam [PTele (SVar (AIdent e)) t] o) = ILam e <$> fromCon t <*> fromCon o
fromCon (SLam (PTele (SVar (AIdent e)) t : l) o) = ILam e <$> fromCon t <*> fromCon (SLam l o)
fromCon (SU (Lv e)) = return (IU (read e))
fromCon s = proofError ("Parsing Error: Cannot interpret " ++ show s ++ "." )

{- Convert Concrete Syntax into Abstract Syntax -}
convert :: Exp -> Proof Term
convert e = fromCon e >>= fromInter . index "" 0
