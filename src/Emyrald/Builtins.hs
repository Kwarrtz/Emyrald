
module Emyrald.Builtins (baseEnv) where

import Emyrald.Error (Error (..))
import Emyrald.Expr (Expr (..), Env, symBase)

addE{-, mulE-}
  :: Expr -> Either Error Expr

addE (Num i) = Right $ Builtin addE'
  where addE' (Num i') = Right $ Num $ i + i'
        addE' _        = Left $ Error "type" "the function '+' was applied to the wrong type"
addE _       = Left $ Error "type" "the function '+' was applied to the wrong type"

-- | The constant 'baseEnv' is an 'Env' of the
--   builtin functions.
baseEnv :: Env
baseEnv =
  [
  (symBase "+", Builtin addE){-,
  (symBase "*", Builtin mulE)-}
  ]
