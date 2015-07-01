
module Emyrald.Error (Error (..)) where

import Data.Char

-- | The type 'Error' is returned in the case of
--   a failed evaluation. It has a single data
--   constructor which takes two arguments: @type@
--   and @message@. @type@ describes the type of error
--   (syntax, value, reference, etc.) while  message
--   contains a synopsis of what caused the error.
data Error =
  Error String String

-- | @show (Error t msg)@ will pretty print the error.
instance Show Error where
  show (Error t msg) = map toUpper t ++ " ERROR\n" ++ msg
