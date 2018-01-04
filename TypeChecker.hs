module TypeChecker where

import PrettyPrinting
import AbstractSyntax
import qualified Data.Map.Strict as Map
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except

infer :: Term -> Proof (Term, Variance)
infer t = do
  wt <- whnf t
  case wt of
    Name i -> do
      tbl <- get
      case Map.lookup i tbl of
           Nothing -> proofError $ "Token " ++ i ++ " not found in context during type inference."
           -- Note: terms in the global context are always covariant.
           Just (_, t)  -> return t
    Var st n -> do
      ctx <- ask
      case (ctx , n) of
        ([], _) -> proofError $ "Cannot infer term variable in empty context."
        ((x, xv):g, 0) -> local tail $ do
                       infer x -- Note, this isn't used, x just needs some type.
                       return (quote x, xv)
        (_:g, n) -> local tail $ do
                       (ty, tyv) <- infer (Var st (n - 1))
                       return (quote ty, tyv)
    tr1 :% tr2 -> do
      (ty1', ty1v) <- infer tr1
      if weakenQ ty1v Plus
      then do
        ty1 <- nwhnf ty1'
        case ty1 of
          Lam vtp1 _ tp1 tp2 -> do
            check tr2 vtp1 tp1
            return (ty1 :% tr2, ty1v) --FIX ME: Should this return Plus instead?
          _ -> proofError $ "Application cannot be performed on non-function."
      else do -- NOTE: This is a speculative change. May need to be removed
        varyNames Minus
        varyCtx Minus $ check tr1 (vComp Minus ty1v) ty1'
        varyNames Minus  -- END NOTE: ...
        ty1 <- nwhnf ty1'
        case ty1 of
          Lam vtp1 _ tp1 tp2 -> do
            check tr2 vtp1 tp1
            return (ty1 :% tr2, ty1v)
          _ -> proofError $ "Application cannot be performed on non-function."
    tr1 :@ tr2 -> undefined
    Lam ty1v s ty1 ty2 -> do
      infer ty1 -- This goes unused. ty1 just needs an inferable type.
      local ((ty1, ty1v):) $ do
        (ty2', ty2'v) <- infer ty2
        if weakenQ ty2'v Plus
        then return (Lam ty1v s ty1 ty2', Plus)
        else proofError $ "Variance error: In order to infer lambda expression, type " ++
                          pshow ty2 ++ " must be (weakenable to) covariant, but instead it's of " ++ show ty2'v ++ " variance."
    U i -> return (U (i + 1), Times) -- Is this right? Should reasoning related to Plus be used?

check :: Term -> Variance -> Term -> Proof ()
check tr Minus ty = do
  varyNames Minus
  varyCtx Minus $ check tr Plus ty
  varyNames Minus
check tr vtr ty =
  case tr of
    Name i -> do
      if weakenQ Plus vtr
      then do
        tbl <- get
        case Map.lookup i tbl of
             Nothing -> proofError $ "Token " ++ i ++ " not found in context during type checking."
             Just (_, (t, _))  -> do
               tynf <- nf ty
               tnf <- nf t
               case (tynf, tnf) of
                 (U j, U i) ->
                   if i <= j
                   then return ()
                   else proofError $ "Size error during global name lookup. " ++ pshow tr ++ " of type "
                                        ++ pshow tnf ++ " is too big for universe " ++ pshow tynf ++ "."
                 _   ->
                   if subTypeQ tynf tnf
                   then return ()
                   else proofError $ "Type didn't match during lookup.  Expected something of type "
                                      ++ pshow tynf ++ "; saw " ++ pshow (Name i) ++ " of type " ++ pshow tnf ++ " instead."
      else proofError $ "Variance error during name check; defined expression " ++ i
                         ++ " is covariant, which can't be weakened to expected variance " ++ show vtr ++ "."
    Var st n -> do
      ctx <- ask
      case (ctx , n) of
        ([], _) -> proofError $ "Cannot check type of variable term in an empty context."
        ((x, xv):g, 0) ->
          if weakenQ xv vtr
          then do
            tynf <- nf ty
            xnf  <- nf (quote x)
            case (tynf, xnf) of
              (U j, U i) ->
                if i <= j
                then return ()
                else proofError $ "Size error during local variable lookup. " ++ pshow tr ++ " of type "
                                    ++ pshow xnf ++ " is too big for universe " ++ pshow tynf ++ "."
              _ ->
                if subTypeQ xnf tynf
                then do 
                  (tyty, tyv) <- infer ty
                  local tail $ check x tyv (unquote tyty)
                else proofError $ "Term does not have correct type. Expected something of type "
                                   ++ pshow tynf ++ "; saw " ++ pshow (Var st n) ++ " of type " ++ pshow xnf ++ " instead."
          else proofError $ "Variance error during variable lookup; variable " ++ st ++ " of type " ++ pshow x ++
                            " is of variance " ++ show xv ++ " which cannot be weakened to expected variance "
                             ++ show vtr ++ "."
        (_:g, _) -> local tail $ check (Var st (n - 1)) vtr (unquote ty)
    Lam atyv ast aty tr' -> do
      tyw <- nwhnf ty
      case tyw of
        Lam vty1 _ ty1 ty2 ->
          if weakenQ vty1 atyv
          then do
            ty1nf <- nf ty1
            atynf <- nf aty
            if subTypeQ ty1nf atynf -- FIX ME: Is this the right order?
            then local ((aty, atyv):) $ check tr' vtr ty2
            else proofError $ "Type of lam annotation didn't match type annotation. Expected "
                               ++ pshow ty1nf ++ "; saw " ++ pshow atynf ++ " instead."
          else proofError $ "Variance error during lambda-lambda check; " ++ ast ++ " of type " ++ pshow aty
                            ++ " and variance " ++ show atyv ++
                            " cannot be weakened to variance " ++ show vty1 ++ "."
        U i -> 
          if weakenQ vtr Plus
          then do
            (atyty, atytyv) <- infer aty
            check aty (f258 atyv) atyty
            local ((aty, Times):) $ check tr' Plus (U i)
            {-
            proofError $ "Variance error during lambda-universe check; hypothesis of type " ++ pshow aty ++
                              " must be some type of variance (weakenable to) " ++ show (f258 atyv) ++
                              ", instead has variance " ++ show atytyv ++ "."
            -}
          else proofError $ "Variance error during lambda-universe check; " ++
                            "lambdas are canonically convenient in the type universe, but " ++ show vtr ++
                            " cannot be weakened to +."
        _ -> proofError $ "Lambdas can only be Lam or Universe types, not " ++ pshow tyw ++ "."
    tr1 :% tr2 -> do
      tynf <- nf ty
      (itynf', itynfv) <- infer (tr1 :% tr2)
      if weakenQ itynfv vtr
      then do
        itynf <- nf itynf'
        case (tynf, itynf) of
          (U j, U i) ->
            if i <= j
            then return ()
            else proofError $ "Size error during application check. " ++ pshow (tr1 :% tr2) ++ " of type "
                                ++ pshow itynf ++ " is too big for universe " ++ pshow tynf ++ "."
          _ ->
            if subTypeQ itynf tynf
            then infer ty >> return () -- Note, this isn't used, ty just needs some type.
            else proofError $ "Failed to unify at application. Expected something of type "
                               ++ pshow tynf ++ "; instead saw " ++ pshow (tr1 :% tr2) ++ " of type " ++
                               pshow itynf ++ "."
      else proofError $ "Variance error during application check; expression " ++ pshow (tr1 :% tr2)
                        ++ " inferred to have variance " ++ show itynfv ++ ", which cannot be weakened to expected variance "
                        ++ show vtr ++ "."
        -- To do: Implement extentionality check.
    tr1 :@ tr2 -> undefined
    U i -> do
        tyw <- nwhnf ty
        case tyw of
          U j -> if i < j then return () else proofError $ "Size error, level " ++ show i ++
                                                            " universe is not a term in the level " ++ show j ++ " universe."
          _ -> proofError $ "Universes can only exist in other universes, not " ++ pshow tyw ++ "."

{- It's not clear that there should be variance checks for universes.
      else proofError $ "Variance error during universe check; universes are covariant, which can't be weakened to expected variance "
                         ++ show vtr ++ "."
-}