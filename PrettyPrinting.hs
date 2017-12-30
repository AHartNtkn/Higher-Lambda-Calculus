module PrettyPrinting where

import Data.Char

import Exp.ErrM
import AbstractSyntax

char :: Int -> String
char i = [chr (i + 97)]

parenA :: Int -> Term -> String
parenA i a@(:%){} = "(" ++ printA i a ++ ")"
parenA i a@Lam{} = "(" ++ printA i a ++ ")"
parenA i a@Pi{} = "(" ++ printA i a ++ ")"
parenA i a = printA i a

printA :: Int -> Term -> String
printA i (Name s) = s
printA i (Var s n) = s
printA i (a :% b) = printA i a ++ " " ++ parenA i b
printA i (Lam st a l@Lam{}) = 
  "(" ++ st ++ " : " ++ printA i a ++ ") " ++ printA (1 + i) l
printA i (Lam st a b) = 
--  if freeIn b 0
  "(" ++ st ++ " : " ++ printA i a ++ ") . " ++ printA (1 + i) b
--  else parenA i a ++ " . " ++ printA (1 + i) b
printA i (Pi st a l@Pi{}) = 
  "(" ++ st ++ " : " ++ printA i a ++ ") " ++ printA (1 + i) l
printA i (Pi st a b) = 
--  if freeIn b 0
  "(" ++ st ++ " : " ++ printA i a ++ ") -> " ++ printA (1 + i) b
--  else parenA i a ++ " . " ++ printA (1 + i) b
printA _ (U i) = "U[" ++ show i ++ "]"

pshow = printA 0

--instance Show Term where
--  show = printA 0

