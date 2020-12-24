{-# LANGUAGE StandaloneDeriving #-}

{-|
Module      : Zozo
Description : Brzozowski derivatives
Copyright   : (c) Nicklas Botö, 2020
License     : GPL-3
Maintainer  : git@nicbot.xyz
Stability   : experimental

Library for constructing, exploring, and visualizing regular expressions and their
@NFA@ form. Focusing heavily on the Brzozowski derivative but also Thompson's construction.
-}

module Zozo
    (
        -- * Language definition
      Regex(..)
    ,

        -- * Regex constructors
      (<|>)
    , (<^>)
    , (***)
    , neg
    ,

        -- ** Specialized constructors
      r
    , some
    , many
    , unit
    ,

        -- * Derivatives
      mem
    , derive
    )
where

import qualified Data.Set                      as S

-- | A simple regex lang
data Regex
        -- | The empty set
        = Nil
        -- | The empty string, \( \epsilon \)
        | Eps
        -- | A symbol as a singleton set
        | Sym   Char
        -- | Union of language sets
        -- \[ \Lambda \vee \Gamma \hspace{10pt} \Lambda,\Gamma \subseteq \Sigma^* \]
        | Union Regex Regex
        -- | Intercept of language sets
        -- \[ \Lambda \wedge \Gamma \hspace{10pt} \Lambda,\Gamma \subseteq \Sigma^* \]
        | Inter Regex Regex
        -- | Complement of language sets
        -- \[ \neg \Gamma \hspace{10pt} \Gamma \subseteq \Sigma^* \]
        | Comp  Regex
        -- | All possible concatenations
        | Conc  Regex Regex
        -- | [Kleene star operator](https://en.wikipedia.org/wiki/Kleene_star)
        | Star  Regex

instance Show Regex where
    show r = '^' : sw r ++ "$"
      where
        sw Nil           = "{}"
        sw Eps           = "ε"
        sw (Sym c      ) = [c]
        sw (Union r1 r2) = '(' : sw r1 ++ "|" ++ sw r2 ++ ")"
        sw (Inter r1 r2) = sw r1 ++ "^" ++ sw r2
        sw (Conc  r1 r2) = sw r1 ++ sw r2
        sw (Comp r     ) = "~(" ++ sw r ++ ")"
        sw (Star r     ) = '(' : sw r ++ ")*"

instance Semigroup Regex where
    (<>) = (***)

instance Monoid Regex where
    mempty = Eps

deriving instance Eq Regex

-- * Regex constructors

-- | Synonym for the @Union@ constructor
infixl 5 <|>
(<|>) :: Regex -> Regex -> Regex
x   <|> Nil = x
Nil <|> y   = y
x   <|> y   = Union x y

-- | Synonym for the @Inter@ constructor
infixl 5 <^>
(<^>) :: Regex -> Regex -> Regex
(<^>) = Inter

-- | Synonym for the @Conc@ constructor
infixl 5 ***
(***) :: Regex -> Regex -> Regex
x   *** Eps = x
Eps *** y   = y
_   *** Nil = Nil
Nil *** _   = Nil
x   *** y   = Conc x y

-- | Synonym for the @Comp@ constructor
neg :: Regex -> Regex
neg = Comp

-- | Treat string as concatenation of symbol regexes
r :: String -> Regex
r = foldMap Sym

-- | Synonym for @Star@ constructor
-- That is, zero or more
some :: Regex -> Regex
some = Star

-- | One or more
many :: Regex -> Regex
many r = some r *** r

-- | 1 function
unit :: Bool -> Regex
unit True  = Eps
unit False = Nil

-- * Set functions

-- | Set membership
mem :: Regex -> Regex -> Bool
mem _ Eps           = False
mem e Nil           = e == Eps
mem e (Sym c      ) = e == Sym c
mem e (Union r1 r2) = mem e r1 || mem e r2
mem e (Inter r1 r2) = mem e r1 && mem e r2
mem e (Conc  r1 r2) = undefined
mem e (Comp r     ) = not $ mem e r
mem e (Star r     ) = undefined

-- | Auxiliary function to derive where
-- \[
-- \nu(R) = \begin{cases}
--          \epsilon \quad & \epsilon \in R\\
--          \varnothing \quad & \text{otherwise} \\
--          \end{cases}
-- \]
-- Equivalent to
-- @
--      unit . (Eps `mem`)
-- @
nu :: Regex -> Regex
nu (Sym c)       = Nil
nu Eps           = Eps
nu Nil           = Nil
nu (Star r     ) = Eps
nu (Conc  r1 r2) = nu r1 <^> nu r2
nu (Inter r1 r2) = nu r1 <^> nu r2
nu (Union r1 r2) = nu r1 <|> nu r2
nu (Comp r     ) = unit (nu r == Nil)

-- * Derivatives

-- | Defining the (Brzozowski) derivative of a language R 
-- with respect to the string \(u \in \Sigma^*\) to be
-- \[
--      u^{-1} L = \left\{ v : uv \in L \right\}
-- \]
derive :: String -> Regex -> Regex
derive s r = foldr deriveSym r (reverse s)

-- | Derivative for individual symbols, to fold over
deriveSym :: Char -> Regex -> Regex
deriveSym _ Eps           = Nil
deriveSym _ Nil           = Nil
deriveSym d (Sym  c     ) = unit (d == c)
deriveSym d (Star r     ) = deriveSym d r *** Star r
deriveSym d (Comp r1    ) = Comp $ deriveSym d r1
deriveSym d (Union r1 r2) = deriveSym d r1 <|> deriveSym d r2
deriveSym d (Inter r1 r2) = deriveSym d r1 <^> deriveSym d r2
deriveSym d (Conc r1 r2) =
    (deriveSym d r1 *** r2) <|> (nu r1 *** deriveSym d r2)

-- | Infix @derive@ with regex symbol input
infixl 4 ^-
(^-) :: String -> Regex -> Regex
(^-) = derive

-- * Evaluation and reduction

-- | Continue reducing a regex
rreduce :: String -> Regex -> Regex
rreduce s = until normal (s ^-) where normal x = derive s x == x

-- | Evaluate regex to a set of strings
eval :: Regex -> S.Set String
eval Nil           = S.empty
eval Eps           = S.singleton ""
eval (Sym c      ) = S.singleton [c]
eval (Union r1 r2) = S.union (eval r1) (eval r2)
eval (Inter r1 r2) = S.intersection (eval r1) (eval r2)
eval (Conc r1 r2) =
    S.map (uncurry (<>)) $ S.cartesianProduct (eval r1) (eval r2)
{-# INLINE eval #-}
