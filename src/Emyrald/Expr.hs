
module Emyrald.Expr
       (
         Expr (..),
         Env,
         display,
         symBase,
         nSymBase,
       ) where

import Emyrald.Error (Error (..))

-- | The 'Expr' type is the haskell representation of
--   Emryald expressions. It has multiple data
--   constructers, each of which represent a different
--   type of expression.
data Expr =
  Num Integer |                         -- ^ The 'Num' constructer represents an integer, which could be considered the only true data type in Emyrald
  SBase String |                        -- ^ The 'SBase' constructer is a helper to the 'Sym' constructer.
  Sym Expr |                            -- ^ The 'Sym' constructer represents a symbol. A "bottom-level" symbol has an 'SBase' as its argument.
  Func Expr Expr |                      -- ^ The 'Func' constructer represents a function.
  Builtin (Expr -> Either Error Expr) | -- ^ The 'Builtin' constructer represents a builtin function, writting in haskell.
  App Expr Expr                         -- ^ The 'App' constructer represents function application.

-- | The 'Eq' instance of 'Expr' supports only three
--   constructers: 'Num', 'SBase' and 'Sym'. An equality
--   check will succeed iff the arguments have the same
--   constructor and arguments, and the constructors are
--   in the above list.
instance Eq Expr where
  (Num i)   == (Num i')   = i == i'
  (SBase s) == (SBase s') = s == s'
  (Sym e)   == (Sym e')   = e == e'
  _         == _          = False

-- | @show \<expr\>@ will return a message displaying
--   the data constructor and the arguments passed to
--   it. It will /not/ pretty print the expression in
--   Emyrald syntax. For that, use 'display'.
instance Show Expr where
  show (Num i)     = "Num { " ++ show i ++ " }"
  show (SBase s)   = "SBase { " ++ s ++ " }"
  show (Sym e)     = "Sym { " ++ show e ++ " }"
  show (Func v e)  = "Func { " ++ show v ++ " } { " ++ show e ++ " }"
  show (Builtin _) = "Builtin {}"
  show (App f a)   = "App { " ++ show f ++ " } { " ++ show a ++ " }"

-- | The function 'display' converts an expression into a
--   string representing that expression in Emyrald syntax.
display :: Expr -> String
display (Num i)     = show i
display (SBase s)   = s
display (Sym e)     = "'" ++ display e
display (Func _ _)  = "<func>"
display (Builtin _) = "<builtin>"
display (App f a)   = display f ++ "[" ++ display a ++ "]"

-- | The function 'symBase' is a shortcut for creating symbols.
--   Writing @symBase str@ is the same as @Sym $ SBase str@.
symBase :: String -> Expr
symBase = Sym . SBase

-- | The function 'nSymBase' is a shortcut for creating symbols.
--   Writing @nSymBase n str@ will return an @n@th degree symbol
--   with base @str@.
nSymBase :: Int -> String -> Expr
nSymBase n str = (iterate Sym $ SBase str) !! n

-- | The type 'Env' represents an Emyrald environment.
--   That is, a collection of expressions bound to symbols.
--   Here, that is represented by an associative list.
type Env = [(Expr, Expr)]
