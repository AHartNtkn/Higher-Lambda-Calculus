module TypeChecker where

import PrettyPrinting
import AbstractSyntax
import qualified Data.Map.Strict as Map
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except

infer :: Term -> Proof Term
infer (Dec tr ty) = do
  check tr ty
  return ty
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
        ([], _) -> proofError $ "Cannot infer term variable " ++ st ++ " in empty context."
        (x:g, 0) -> local tail $ do
                       infer x -- Note, this isn't used, x just needs some type.
                       return (quote x)
        (_:g, n) -> local tail $ do
                       ty <- infer (Var st (n - 1))
                       return (quote ty)
    tr1 :% tr2 -> do
      ty1 <- infer tr1
      inty <- fun tr1 `catchError` (\e -> proofError $ e ++ "; Error encountered when assessing " ++ pshow tr1 ++ " : " ++ pshow ty1 ++ "." )
      check tr2 inty
      return (ty1 :% tr2)
    Lam s ty1 ty2 -> do
      infer ty1 -- This goes unused. ty1 just needs an inferable type.
      local (ty1:) $ do
        ty2' <- infer ty2
        return $ Lam s ty1 ty2'
    U i -> return (U (i + 1))

check :: Term -> Term -> Proof ()
check tr ty =
  case tr of
    Dec tr' ty' -> do
      tynf <- nf ty
      tynf' <- nf ty'
      if tynf == tynf'
      then check tr' ty
      else proofError $ "Type decoration " ++ pshow tr ++ " does not match expected type " ++ pshow ty ++ "."
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
               (U j, _) -> do
                  luaty <- lowestU tnf
                  if luaty <= j
                  then return ()
                  else proofError $ "Expression " ++ pshow tr ++ " is too big for universe " ++ pshow (U j) ++
                           ", it resides within " ++ pshow (U luaty) ++ " during name lookup."
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
            (U j, _) -> do
               luaty <- lowestU xnf
               if luaty <= j
               then return ()
               else proofError $ "Expression " ++ pshow tr ++ " is too big for universe " ++ pshow (U j) ++
                           ", it resides within " ++ pshow (U luaty) ++ " during name lookup."
            _ ->
              if tynf == xnf
              then do 
                tyty <- infer ty
                local tail $ check x (unquote tyty)
              else proofError $ "Term does not have correct type. Expected something of type "
                                 ++ pshow tynf ++ "; saw " ++ pshow (Var st n) ++ " of type " ++ pshow xnf ++ " instead."
        (_:g, _) -> local tail $ check (Var st (n - 1)) (unquote ty)
    Lam s aty tr' -> do
      tyw <- nwhnf ty
      case tyw of
        Lam _ ty1 ty2 -> do
          ty1nf <- nf ty1
          atynf <- nf aty
          if ty1nf == atynf
          then local (ty1:) $ check tr' ty2
          else proofError $ "Type of lam annotation didn't match type annotation. Expected "
                             ++ pshow ty1nf ++ "; saw " ++ pshow atynf ++ " instead."
        U i -> do
          luaty <- lowestU tr
          if luaty <= i
          then return ()
          else proofError $ "Lambda expression " ++ pshow tr ++ " is too big for universe " ++ pshow (U i) ++
                   ", it resides within " ++ pshow (U luaty) ++ "."
        a -> check tr (Lam s aty (quote a :% Var s 0)) -- Implement eta-equivalence type-checking
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
    U i -> do
      tyw <- nwhnf ty
      case tyw of
        U j -> if i < j then return () else proofError $ "Size error, level " ++ show i ++
                                                          " universe is not a term in the level " ++ show j ++ " universe."
        _ -> proofError $ "Universes can only exist in other universes, not " ++ pshow tyw ++ "."

-- Calculate the lowest universe a term resides within.
-- It seems like this should be used elsewhere. Hmm...
lowestU :: Term -> Proof Int
lowestU tr = nwhnf tr >>= lamgo where
  lamgo (U i) = return (i + 1)
  lamgo (Lam _ aty tr') = local (aty:) $ lamgo tr'
  lamgo tr = do
    tri <- nwhnf =<< infer tr
    case tri of
      U i -> return i
      ty -> lowestU ty

-- Calculate if a term is functional, and return the expected type of input.
fun :: Term -> Proof Term
fun tr = do
  trw <- nwhnf tr
  case trw of
    Lam _ aty tr' -> return aty
    U i -> proofError $ "Error during functional check: term " ++ pshow tr ++ " is not functional;" ++
                " Application cannot be performed on non-function."
    tr -> do
      itr <- infer tr
      fun itr
