module AbstractSyntax where

import qualified Data.Map.Strict as Map
import Control.Monad.Reader hiding (liftIO)
import Control.Monad.State hiding (liftIO)
import Control.Monad.Except hiding (liftIO)
import Control.Monad.Trans.Except hiding (liftIO)
-- import Exp.ErrM

type Proof = ExceptT String (ReaderT [Term] (StateT (Map.Map String (Term, Term)) IO))

runProof :: Proof a -> IO (Either String a, Map.Map String (Term, Term))
runProof p = runStateT (runReaderT (runExceptT p) []) Map.empty

liftIO :: IO a -> Proof a
liftIO act = lift $ lift $ lift act

proofError :: String -> Proof a
proofError s = do
  liftIO $ putStrLn s
  throwError s

{- Unannotated Terms -}
infixl 9 :%
data Term
    = Lam String Term Term
    | Var String Int
    | Name String
    | Dec Term Term -- Decorate term with type
    | Term :% Term
    | U Int deriving (Show)

instance Eq Term where
  Lam _ a b == Lam _ a' b' = a == a' && b == b'
  Var _ i == Var _ j = i == j
  Name i == Name j = i == j
  Dec t _ == Dec t' _ = t == t'
  a :% b == a' :% b' = a == a' && b == b'
  U i == U j = i == j
  _ == _ = False

-- Increment free variables
quote' n (Var s x)   = if x >= n then Var s (1 + x) else Var s x
quote' n (Lam s t d) = Lam s (quote' n t) (quote' (1 + n) d)
quote' n (d :% b)    = quote' n d :% quote' n b
quote' n (Dec d b)   = Dec (quote' n d) (quote' n b)
quote' n x           = x

quote = quote' 0

unquote = sub (Var "" 0) 0

sub s n (Var st x) =
  case x `compare` n of
    GT -> Var st (x - 1)
    EQ -> s
    LT -> Var st x
sub s n (Lam st t d) = Lam st (sub s n t) (sub (quote s) (1 + n) d)
sub s n (d :% b)  = sub s n d :% sub s n b
sub s n (Dec d b) = Dec (sub s n d) (sub s n b)
sub s n x         = x

-- Reduce a term to weak head normal form.
whnf' :: Bool -> Term -> Proof Term
whnf' names ee = spine ee [] where
  spine :: Term -> [Term] -> Proof Term
  spine (f :% x) xs = spine f (x:xs)
  spine (Dec x b) xs = spine x xs -- Is this correct?
  spine (Lam s t z) (u:xs) = spine (sub u 0 z) xs
  spine (Name i) xs = 
    if names -- Should names/levels be removed
    then do
      tbl <- get
      case Map.lookup i tbl of
        Nothing -> proofError $ "Token " ++ show i ++ " not found in context (whnf)."
        Just t  -> spine (fst t) xs
    else app (Name i) xs
  spine f xs = app f xs
  app f xs = return $ foldl (:%) f xs

whnf = whnf' False
nwhnf = whnf' True

-- Note: Eta-equivalence breaks lambda-pi merging.
-- Normal Form
nf' :: Term -> Proof Term
nf' ee = spine ee [] where
  spine (f :% x) xs = spine f (x:xs)
  spine (Dec x b) xs = spine x xs
  spine (Lam st t e) [] = Lam st <$> nf' t <*> nf' e
  spine (Lam st t e) (u:xs) = spine (sub u 0 e) xs
  spine (Name s) xs = do
        tbl <- get
        case Map.lookup s tbl of
          Nothing -> proofError $ "Token " ++ show s ++ " not found in context."
          Just t  -> spine (fst t) xs
  spine f xs = foldl (:%) f <$> mapM nf' xs

nf d = do
  r <- nf' d
  if d == r
  then return r
  else nf r
