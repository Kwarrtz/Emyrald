
module Emyrald.Eval (evalExpr{-, evalProg-}) where

import Emyrald.Error (Error (..))
import Emyrald.Expr (Expr (..), Env)

-- | The function 'evalExpr' evaluates an 'Expr'
--   within the global context of the provided
--   'Env'.
evalExpr :: Env -> Expr -> Either Error Expr
-- self evaluating expressions
evalExpr _ (Num i)     = Right $ Num i
evalExpr _ (SBase s)   = Right $ SBase s
evalExpr _ (Func v ex) = Right $ Func v ex
evalExpr _ (Builtin f) = Right $ Builtin f
-- symbols
evalExpr e (Sym (SBase s)) = maybe (Left $ Error "reference" $ "'" ++ s ++ "' is not in scope") Right $ lookup (Sym $ SBase s) e
evalExpr _ (Sym ex)        = Right ex
-- function application
evalExpr _ (App (Builtin f) a)     = f a
evalExpr e (App (Func v ex) a)     = evalExpr (e ++ [(v, a)]) ex
evalExpr e (App (Sym (SBase s)) a) = evalExpr e $ App (Sym $ SBase s) a
evalExpr e (App (Sym ex) a)        = (\x->evalExpr e $ App x a) =<< evalExpr e (Sym ex)
evalExpr e (App (App f' a') a)     = (\ex->evalExpr e $ App ex a) =<< evalExpr e (App f' a')
evalExpr _ (App (Num _) _)         = Left $ Error "type" "'int' objects are not callable"
-- otherwise
evalExpr _ _ = Left $ Error "unknown" "invalid expression"
