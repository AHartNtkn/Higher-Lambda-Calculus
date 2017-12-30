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
        Pi _ tp1 tp2 -> do
          check tr2 tp1
          return (sub tr2 0 tp2)
        _ -> proofError $ "Application cannot be performed on non-function " ++ pshow tr1 ++ "."
    Pi s ty1 ty2 -> do
      infer ty1 -- This goes unused. ty1 just needs an inferable type.
      local (ty1:) $ do
        ty2' <- nwhnf =<< infer ty2
        case ty2' of
          U i -> return (U i)
          _   -> return (U 0)
    Lam s ty1 ty2 -> do
      infer ty1 -- This goes unused. ty1 just needs an inferable type.
      local (ty1:) $ do
        ty2' <- infer ty2
        return $ Pi s ty1 ty2'
    U i -> return (U (i + 1))

check :: Term -> Term -> Proof ()
check tr ty =
  case tr of
    Name i -> do
      tbl <- get
      case Map.lookup i tbl of
           Nothing -> proofError $ "Token " ++ i ++ " not found in context during type checking."
           Just (_, t)  -> do
             tynf <- nf ty
             tnf <- nf t
             case (tynf, tnf) of
               (U j, U i) ->
                 if i <= j
                 then return ()
                 else proofError $ "Size error during global name lookup. " ++ pshow tr ++ " of type "
                                      ++ pshow tnf ++ " is too big for universe " ++ pshow tynf ++ "."
               _   -> do
                 if tnf == tynf
                 then return ()
                 else proofError $ "Type didn't match during lookup.  Expected something of type "
                                    ++ pshow tynf ++ "; saw " ++ pshow (Name i) ++ " of type " ++ pshow tnf ++ " instead."
    Var st n -> do
      ctx <- ask
      case (ctx , n) of
        ([], _) -> proofError $ "Cannot check type of variable term in an empty context."
        (x:g, 0) -> do
          tynf <- nf ty
          xnf  <- nf (quote x)
          case (tynf, xnf) of
            (U j, U i) ->
              if i <= j
              then return ()
              else proofError $ "Size error during local variable lookup. " ++ pshow tr ++ " of type "
                                  ++ pshow xnf ++ " is too big for universe " ++ pshow tynf ++ "."
            _ ->
              if tynf == xnf
              then do 
                tyty <- infer ty
                local tail $ check x (unquote tyty)
              else proofError $ "Term does not have correct type. Expected something of type "
                                 ++ pshow tynf ++ "; saw " ++ pshow (Var st n) ++ " of type " ++ pshow xnf ++ " instead."
        (_:g, _) -> local tail $ check (Var st (n - 1)) (unquote ty)
    Pi s aty rty -> do
      tyw <- nwhnf ty
      case tyw of
        U i -> do
          infer aty -- This goes unused. aty just needs an inferable type.
          rtyty <- nwhnf =<< local (aty:) (infer rty)
          case rtyty of
            U j -> if j <= i
                   then return ()
                   else proofError $ "Size error during pi check. " ++ pshow (rty) ++ " of type "
                              ++ pshow rtyty ++ " is too big for universe " ++ pshow tyw ++ "."
            _ -> return () -- Something else should be done here.
        _ -> proofError $ "Pi types can only be within a universe, not " ++ pshow tyw ++ "."
    Lam _ aty tr' -> do
      tyw <- nwhnf ty
      case tyw of
        Pi _ ty1 ty2 -> do
          ty1nf <- nf ty1
          atynf <- nf aty
          if ty1nf == atynf
          then local (ty1:) $ check tr' ty2
          else proofError $ "Type of lam annotation didn't match type annotation. Expected "
                             ++ pshow ty1nf ++ "; saw " ++ pshow atynf ++ " instead."
        _ -> proofError $ "Lambdas can only be Pi types, not " ++ pshow tyw ++ "."
    tr1 :% tr2 -> do
      tynf <- nf ty
      itynf <- nf =<< infer (tr1 :% tr2)
      case (tynf, itynf) of
        (U j, U i) ->
          if i <= j
          then return ()
          else proofError $ "Size error during application check. " ++ pshow (tr1 :% tr2) ++ " of type "
                              ++ pshow itynf ++ " is too big for universe " ++ pshow tynf ++ "."
        _ ->
          if tynf == itynf
          then infer ty >> return () -- Note, this isn't used, ty just needs some type.
          else proofError $ "Failed to unify at application. Expected something of type "
                             ++ pshow tynf ++ "; instead saw " ++ pshow (tr1 :% tr2) ++ " of type " ++
                             pshow itynf ++ "."
    U i -> do -- What to do to implement proper kinds?
      tyw <- nwhnf ty
      case tyw of
        U j -> if i < j then return () else proofError $ "Size error, level " ++ show i ++
                                                          " universe is not a term in the level " ++ show j ++ " universe."
        _ -> proofError $ "Universes can only exist in other universes, not " ++ pshow tyw ++ "."
