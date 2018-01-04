module AbstractSyntax where

import qualified Data.Map.Strict as Map
import Control.Monad.Reader hiding (liftIO)
import Control.Monad.State hiding (liftIO)
import Control.Monad.Except hiding (liftIO)
import Control.Monad.Trans.Except hiding (liftIO)
-- import Exp.ErrM

-- =*=*=*=*=*=*=*=* Proof Environment Monad *=*=*=*=*=*=*=*=*=

type Proof = ExceptT String (ReaderT [(Term, Variance)] (StateT (Map.Map String (Term, (Term, Variance))) IO))

runProof :: Proof a -> IO (Either String a, Map.Map String (Term, (Term, Variance)))
runProof p = runStateT (runReaderT (runExceptT p) []) Map.empty

liftIO :: IO a -> Proof a
liftIO act = lift $ lift $ lift act

proofError :: String -> Proof a
proofError s = do
  liftIO $ putStrLn s
  throwError s

-- =*=*=*=*=*=*=*=* Variance *=*=*=*=*=*=*=*=*=

data Variance
  = Plus
  | Minus
  | Times
  | Iso deriving (Eq)

instance Show Variance where
  show Plus  = "+"
  show Minus = "-"
  show Times = "*"
  show Iso   = "="

vComp :: Variance -> Variance -> Variance
vComp _ Iso = Iso
vComp Iso _ = Iso
vComp Times _ = Times
vComp _ Times = Times
vComp Minus Plus = Minus
vComp Plus Minus = Minus
vComp _ _ = Plus

-- See if a Variance can be weakened into another
weakenQ ::  Variance -> Variance -> Bool
weakenQ Times _ = True
weakenQ _ Iso = True
weakenQ x y = x == y

-- Calculate what needs to be composed for weakening.
-- Assumes weakenQ is already used
weaken :: Variance -> Variance -> Variance
weaken Plus a = a
weaken Minus Minus = Plus
weaken Minus Plus = Minus
weaken Minus a = a
weaken Times Times = Plus
weaken Times Iso = Iso
weaken Iso Iso = Plus
-- Everything else should be an error.

-- A function used in a calculation when checking if a lambda exression is in the type universe;
f258 :: Variance -> Variance
f258 Plus = Minus
f258 Minus = Times
f258 Times = Times
f258 Iso = Minus

varyCtx :: Variance -> Proof a -> Proof a
varyCtx v = local (map (\(t, v') -> (t, vComp v v')))

varyNames :: Variance -> Proof ()
varyNames v = do
  tbl <- get
  put (Map.map (\(tr,(ty,tv)) -> (tr,(ty,vComp v tv))) tbl)
  return ()

-- =*=*=*=*=*=*=*=* Syntax *=*=*=*=*=*=*=*=*=

infixl 9 :%
data Term
    = Lam Variance String Term Term
    | Var String Int
    | Name String
    | Term :% Term
    | Term :@ Term
    | U Int deriving (Show)

instance Eq Term where
  Lam v _ a b == Lam v' _ a' b' = v == v' && a == a' && b == b'
  Var _ i == Var _ j = i == j
  Name i == Name j = i == j
  a :% b == a' :% b' = a == a' && b == b'
  a :@ b == a' :% b' = a == a' && b == b'
  U i == U j = i == j
  _ == _ = False

-- Check if one type is a subtype of another.
subTypeQ :: Term -> Term -> Bool
subTypeQ (Lam v _ a b) (Lam v' _ a' b') = weakenQ v' v && subTypeQ a a' && subTypeQ b b'
subTypeQ (a :% b) (a' :% b') = subTypeQ b b'
subTypeQ (U i) (U j) = i <= j
subTypeQ x y = x == y

-- =*=*=*=*=*=*=*=* Computation *=*=*=*=*=*=*=*=*=

-- Check if a variable occures freely in a term
freeIn (Var s x)  n = x == n
freeIn (d :% d1)  n = freeIn d n || freeIn d1 n
freeIn (d :@ d1)  n = freeIn d n || freeIn d1 n
freeIn (Lam _ _ t tp) n = freeIn t n || freeIn tp (1 + n)
freeIn _          n = False

-- Increment free variables
quote' n (Var s x)   = if x >= n then Var s (1 + x) else Var s x
quote' n (Lam v s t d) = Lam v s (quote' n t) (quote' (1 + n) d)
quote' n (d :% b)    = quote' n d :% quote' n b
quote' n (d :@ b)    = quote' n d :% quote' n b
quote' n x           = x

quote = quote' 0

unquote = sub (Var "" 0) 0

sub s n (Var st x) =
  case x `compare` n of
    GT -> Var st (x - 1)
    EQ -> s
    LT -> Var st x
sub s n (Lam v st t d) = Lam v st (sub s n t) (sub (quote s) (1 + n) d)
sub s n (d :% b)  = sub s n d :% sub s n b
sub s n (d :@ b)  = sub s n d :@ sub s n b
sub s n x         = x

-- Reduce a term to weak head normal form.
whnf' :: Bool -> Term -> Proof Term
whnf' names ee = spine ee [] where
  spine :: Term -> [Term] -> Proof Term
  spine (f :% a) as = spine f (a:as)
  spine (f :@ a) as = spine f (a:as)
  spine (Lam v s t z) (u:as) = spine (sub u 0 z) as
  spine (Name i) as = 
    if names -- Should names/levels be removed
    then do
      tbl <- get
      case Map.lookup i tbl of
        Nothing -> proofError $ "Token " ++ show i ++ " not found in context (whnf)."
        Just t  -> spine (fst t) as
    else app (Name i) as
  spine f as = app f as
  app f as = return $ foldl (:%) f as

whnf = whnf' False
nwhnf = whnf' True

-- Note: Eta-equivalence breaks lambda-pi merging.
-- Normal Form
nf' :: Term -> Proof Term
nf' ee = spine ee [] where
  spine (f :% a) as = spine f (a:as)
  spine (f :@ a) as = spine f (a:as)
  spine (Lam v st t e) [] = Lam v st <$> nf' t <*> nf' e
  spine (Lam v st t e) (u:as) = spine (sub u 0 e) as
  spine (Name s) as = do
        tbl <- get
        case Map.lookup s tbl of
          Nothing -> proofError $ "Token " ++ show s ++ " not found in context."
          Just t  -> spine (fst t) as
  spine f as = foldl (:%) f <$> mapM nf' as

nf d = do
  r <- nf' d
  if d == r
  then return r
  else nf r
