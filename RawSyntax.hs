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
   | ILam String ITerm ITerm
   | IPi String ITerm ITerm
   | IApp ITerm ITerm
   | IStar
   deriving (Show)

{- Convert intermediate syntax into abstract syntax -}

-- Replace string variables with deBruijin indices
index :: String -> Int -> ITerm -> ITerm
index s n (IVS x) = if s == x then IV x n else IVS x
index s n (IV st x) = IV st x
index s n (ILam x d d1) = ILam x (index s n d) (index s (1 + n) (index x 0 d1))
index s n (IPi x d d1) = IPi x (index s n d) (index s (1 + n) (index x 0 d1))
index s n (IApp d d1) = IApp (index s n d) (index s n d1)
index s n IStar = IStar

-- Convert intermediate syntax into abstract syntax
fromInter :: ITerm -> Proof Term
fromInter (IV s x) = return $ Var s x
fromInter (IVS x) = return $ Name x
fromInter (IApp d d1) = (:%) <$> fromInter d <*> fromInter d1
fromInter (ILam x d d1) = Lam x <$> fromInter d <*> fromInter d1
fromInter (IPi x d d1) = Pi x <$> fromInter d <*> fromInter d1
fromInter IStar = return U

{- Convert concrete syntax into intermediate syntax -}
fromCon :: Exp -> Proof ITerm
fromCon (SLet d e) = proofError "TO DO: Implement let expressions"
fromCon (SApp a b) = IApp <$> fromCon a <*> fromCon b
fromCon (SVar (AIdent e)) = return $ IVS e
fromCon (SLam [PTele (SVar (AIdent e)) t] o) = ILam e <$> fromCon t <*> fromCon o
fromCon (SLam (PTele (SVar (AIdent e)) t : l) o) = ILam e <$> fromCon t <*> fromCon (SLam l o)
fromCon (SPi [PTele (SVar (AIdent e)) t] o) = IPi e <$> fromCon t <*> fromCon o
fromCon (SPi (PTele (SVar (AIdent e)) t : l) o) = IPi e <$> fromCon t <*> fromCon (SLam l o)
fromCon SU = return IStar
fromCon s = proofError ("Parsing Error: Cannot interpret " ++ show s ++ "." )

{- Convert Concrete Syntax into Abstract Syntax -}
convert :: Exp -> Proof Term
convert e = fromCon e >>= fromInter . index "" 0
