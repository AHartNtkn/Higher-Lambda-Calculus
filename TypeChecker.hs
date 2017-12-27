module TypeChecker where

import PrettyPrinting
import AbstractSyntax
import qualified Data.Map.Strict as Map
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except

infer :: Term -> Proof Term
infer t = do
  wt <- whnf t
  case wt of
    Name i -> do
      tbl <- get
      case Map.lookup i tbl of
           Nothing -> proofError $ "Token " ++ i ++ " not found in context during type inference."
           Just t  -> return $ snd t
    Var st n -> do
      ctx <- ask
      case (ctx , n) of
        ([], _) -> proofError $ "Cannot infer term variable in empty context."
        (x:g, 0) -> local tail $ do
                       infer x -- Note, this isn't used, x just needs some type.
                       return (quote x)
        (_:g, n) -> local tail $ do
                       ty <- infer (Var st (n - 1))
                       return (quote ty)
    tr1 :% tr2 -> do
      ty1 <- nwhnf =<< infer tr1
      case ty1 of
        Lam _ tp1 tp2 -> do
          check tr2 tp1
          return (ty1 :% tr2)
        _ -> proofError $ "Application cannot be performed on non-function."
    Lam s ty1 ty2 -> do
      infer ty1 -- This goes unused. ty1 just needs an inferable type.
      local (ty1:) $ do
        ty2' <- infer ty2
        return $ Lam s ty1 ty2'
    U -> return Kind
    Kind -> proofError $ "Kinds cannot be typed."

check :: Term -> Term -> Proof ()
check tr ty =
  case tr of
    Name i -> do
      tbl <- get
      case Map.lookup i tbl of
           Nothing -> proofError $ "Token " ++ i ++ " not found in context during type checking."
           Just (_, t)  -> do
             tnf  <- nf t
             tynf <- nf ty
             if tnf == tynf
             then return ()
             else proofError $ "Type didn't match during lookup.  Expected something of type "
                                ++ pshow ty ++ "; saw " ++ pshow (Name i) ++ " of type " ++ pshow t ++ " instead."
    Var st n -> do
      ctx <- ask
      case (ctx , n) of
        ([], _) -> proofError $ "Cannot check type of variable term in an empty context."
        (x:g, 0) -> do
          xnf  <- nf (quote x)
          tynf <- nf ty
          if tynf == xnf
          then do 
            tyty <- infer ty
            local tail $ check x (unquote tyty)
          else proofError $ "Term does not have correct type. Expected something of type "
                             ++ pshow ty ++ "; saw " ++ pshow (Var st n) ++ " of type " ++ pshow x ++ " instead."
        (_:g, _) -> local tail $ check (Var st (n - 1)) (unquote ty)
    Lam _ aty tr' -> do
      tyw <- nwhnf ty
      case tyw of
        Lam _ ty1 ty2 -> do
          ty1nf <- nf ty1
          atynf <- nf aty
          if ty1nf == atynf
          then local (ty1:) $ check tr' ty2
          else proofError $ "Type of lam annotation didn't match type annotation. Expected "
                             ++ pshow ty1 ++ "; saw " ++ pshow aty ++ " instead."
        U -> do
          infer aty -- This goes unused. aty just needs an inferable type.
          local (aty:) $ check tr' U
        Kind -> do
          infer aty -- This goes unused. aty just needs an inferable type.
          local (aty:) $ check tr' Kind
        _ -> proofError $ "Lambdas can only be Lam or * types."
    tr1 :% tr2 -> do
      ity <- infer (tr1 :% tr2)
      tynf <- nf ty
      itynf <- nf ity
      if tynf == itynf
      then infer ty >> return () -- Note, this isn't used, ty just needs some type.
      else proofError $ "Failed to unify at application. Expected something of type "
                         ++ pshow ty ++ "; instead saw " ++ pshow (tr1 :% tr2) ++ " of type " ++
                         pshow ity ++ "."
    U -> do -- What to do to implement proper kinds?
      tyw <- nwhnf ty
      case tyw of
        Kind -> return ()
        _ -> proofError $  "* can only be a Kind."
