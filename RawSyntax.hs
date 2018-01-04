module RawSyntax where

import AbstractSyntax
import PrettyPrinting

import Control.Monad.Except

-- Representation for printing
import Exp.Abs

import Control.Monad
import Data.String

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM

{- Intermediate raw syntax -}
data ITerm
   = IVS String
   | IV String Int
   | ILam Variance String ITerm ITerm
   | IApp ITerm ITerm
   | IPAp ITerm ITerm
   | IU Int
   deriving (Show)

{- Convert intermediate syntax into abstract syntax -}

-- Replace string variables with deBruijin indices
index :: String -> Int -> ITerm -> ITerm
index s n (IVS x) = if s == x then IV x n else IVS x
index s n (IV st x) = IV st x
index s n (ILam v x d d1) = ILam v x (index s n d) (index s (1 + n) (index x 0 d1))
index s n (IApp d d1) = IApp (index s n d) (index s n d1)
index s n (IPAp d d1) = IPAp (index s n d) (index s n d1)
index s n (IU i) = IU i

-- Convert intermediate syntax into abstract syntax
fromInter :: ITerm -> Proof Term
fromInter (IV s x) = return $ Var s x
fromInter (IVS x) = return $ Name x
fromInter (IApp d d1) = (:%) <$> fromInter d <*> fromInter d1
fromInter (IPAp d d1) = (:@) <$> fromInter d <*> fromInter d1
fromInter (ILam v x d d1) = Lam v x <$> fromInter d <*> fromInter d1
fromInter (IU i) = return (U i)

{- Convert concrete syntax into intermediate syntax -}
fromCon :: Exp -> Proof ITerm
fromCon (SLet d e) = proofError "TO DO: Implement let expressions"
fromCon (SApp a b) = IApp <$> fromCon a <*> fromCon b
fromCon (SPAp a b) = IPAp <$> fromCon a <*> fromCon b
fromCon (SVar (AIdent e)) = return $ IVS e
fromCon (SLam [p] o) = (case p of
  PPls (SVar (AIdent e)) t -> ILam Plus e <$> fromCon t
  PMin (SVar (AIdent e)) t -> ILam Minus e <$> fromCon t
  PTim (SVar (AIdent e)) t -> ILam Times e <$> fromCon t
  PIso (SVar (AIdent e)) t -> ILam Iso e <$> fromCon t) <*> fromCon o
fromCon (SLam (p : l) o) = (case p of
  PPls (SVar (AIdent e)) t -> ILam Plus e <$> fromCon t
  PMin (SVar (AIdent e)) t -> ILam Minus e <$> fromCon t
  PTim (SVar (AIdent e)) t -> ILam Times e <$> fromCon t
  PIso (SVar (AIdent e)) t -> ILam Iso e <$> fromCon t) <*> fromCon (SLam l o)
fromCon (SU (Lv e)) = return (IU (read e))
fromCon s = proofError ("Parsing Error: Cannot interpret " ++ show s ++ "." )

{- Convert Concrete Syntax into Abstract Syntax -}
convert :: Exp -> Proof Term
convert e = fromCon e >>= fromInter . index "" 0
