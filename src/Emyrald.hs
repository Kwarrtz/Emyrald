

{-|
Module      : Emyrald
Description : The Emyrald Language v0.1.0.0
Copyright   : © 2015 Dathan Ault-McCoy
License     : GPL-3
Stability   : experimental
Portability : POSIX



Emyrald is a lightweight, functional, symbolic, and homoiconic
programming language inspired by Scheme and Haskell. Its goal
is to create the first completely functional homoiconic language.

This library defines a set of data types and functions for the
parsing and evaluation of Emyrald expressions and programs.It has
been divided into five sub-modules:

 * Error: defines the 'Error' data type for errors in evaluation
 * Expr: defines the 'Expr' data type for representing Emyrald
   data types in Haskell, as well as functions for working with it.
 * Eval: provides functions for evaluating 'Expr's
 * Builtins: defines all Emyrald builtin functions, exported as
   'baseEnv'.
 * Parse: provides functions for parsing a string into 'Expr's.

=Background

Not long after I first began to program, I was introduced to the
family of languages collectively known as Scheme. I was immediately
taken by its elegance and simplicity. I was fascinated by its
functionality and homoiconicity in particular. It quickly became
my favorite language and I devoted much of my time spent programming
to exploring it. Several years later I was introduced to Haskell.
Once again, I was taken. I became mesmerized by curried functions
and monads, and the inticacies of Haskell's typing system. Slowly
but surely, I began to shift my focus from Scheme to this new and
exciting language.

Between my introduction to Haskell and severa articles that I ran
accros online, I began to see some of Scheme's faults. Obvious
issues, like the ease with which you can evaluate an expression
"too much" or "too little", and subtle things, like how numbers
and strings are self evaluating but lists aren't. I also began to
find myself missing many of the features I had grown used to in
Haskell, such as partial application, function composition, and
monads. What was really the nail in the coffin, though, was that
Scheme isn't /actually/ a functional language; its functions can
have side effects.

But even as a drifted away from Scheme and towards Haskell, through
every function I composed and monad I bound, there was this constant
nagging feeling in the back of my mind. After Scheme's macros and
dynamic types, Haskell's statements felt messy and its complicated
typing system clumsy and unnecesary. And, it wasn't homoiconic. For
all Haskell's elegance, it lacked Scheme's simplicity and symmetry
that was my first love.

I was conflicted, badly so. I just didn't know what to do. Should I
go for Haskell's elegance and brevity? Or should I stick to Scheme's
simplicity and symmetry? Then I slowly began to wonder, why should
I have to choose? Why not both? Is there some physical law that
prevents a unity of the two? No, there isn't! And thus began my
quest to create the perfect language, an idealogical unity of Scheme
and Haskell. This is the quest that has led me to create Emyrald.
Although it is an early attempt, I hope it paves the way for future,
more succesful attempts.


=Philosophy

Emyrald is an attempt to combin the best of Scheme (homoiconicity,
syntactic simplicity, and a simple type system) with the best of
Haskell (brevity, purely-functional curried functions, lazy
evaluation and computational contexts). This means no statements,
strict typing or complicated "syntactic sugar".


=Types

Emyrald has only three types: integers, symbols and functions. All
other "types" are implemented using structs which are, in turn,
implemented using self-evaluating functions.

===Integers

Integers are arbitrary precision but are otherwise as you would find
them in any other language. They are analagous to Scheme's @Bignum@
or Haskell's @Integer@.

===Symbols

Symbols in Emyrald are similar to symbols in LISP or Scheme. They are
effectively strings with different evaluation rules. Unlike in Scheme,
the @quote@ function has been built into symbols. Where @''foo@ would
parses to @(quote (quote foo))@ in Scheme, it parses to @Sym $ Sym $
SBase "foo"@ in Emyrald. In otherwords, symbols in Emyrald effectively
take an argument, which can either be another symbol, or an "SBase". A
symbol which has an "SBase" as its argument is a "bottom-level" symbol,
analagous to an unquoted symbol in Scheme. Evaluating @'foo@ yields
@foo@, and evaluating @foo@ returns whatever expression is bound to
@foo@. As such, symbols are used as variables in Emyrald.

===Functions

Functions in Emyrald are curried, so they take only one argument. A
function, @f@ has two parts, a symbol, @a@ and an expression, @e@. When
@f@ is applied to argument @x@, @x@ is bound to @a@ within the scope of
@f@ and @e@ is evaluated. There are convenience functions for constructing
functions of multiple arguments.

===Lists

Lists are implemented using the builtin, two argument, self-evaluating
@cons@ function. It acts similar to the @(:)@ constructor in Haskell.
Emyrald supports all of the regular list manipulation functions.

===Characters and Strings

Unlike in most languages, Emyrald has no concept of characters or strings.
Characters are instead represented as their ascii (or unicode) code and
strings are lists of characters. Whether a number represents a number or
a character is inferred from the function that it is being passed to.

===Floats

Floating point numbers are implemented using structs. The builtin function
@float@, like @cons@, takes to arguments and is self evaluating. To make
operations of floats more efficient, they are converted into Haskell's
@Double@ for arithmetic operations.


=Syntax

Emyralds syntax is very simple and completely whitespace insensitive.

===Literals

The syntaxes for literals are:

 * Integers: @42@, @-7@
 * Symbols: @foo@, @'bar@, @''baz@, etc.

Emyrald also specifies convenience syntaxes for chars, strings, lists
and floats:

 * Characters: @&c@, @&"@, @&\\newline@, @&\\space@
 * Strings: @"Hello World!"@
 * Lists: @(7, "foo", 'symbol)@, @()@
 * Floats: @3.7@, @-2.0@

==Function Application

Functions are applied by @f[x]@. Function application is left associative,
and since Emyrald functions are curried, @f[x][y][z]@ applies multi argument
functions. For convenience, @f[x][y][z]@ can be written @f[x,y,z]@.

-}

module Emyrald
       (
         Error,
         Expr (..),
         Env,
         display,
         symBase,
         nSymBase,
         evalExpr,
         {-evalProg,-}
         baseEnv,
         parseExpr,
         parseProg
       ) where

import Emyrald.Error
import Emyrald.Expr
import Emyrald.Eval
import Emyrald.Builtins
import Emyrald.Parse
