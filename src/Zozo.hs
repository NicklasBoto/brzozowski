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
module Zozo where

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
        -- \[ \{a\}\{b\} := \{a, b, ab, ba\} \]
        | Conc  Regex Regex
        -- | [Kleene star operator](https://en.wikipedia.org/wiki/Kleene_star)
        | Star  Regex

-- * Regex constructors

-- | Synonym for the @Union@ constructor
(<|>) :: Regex -> Regex -> Regex
(<|>) = Union

-- | Synonym for the @Inter@ constructor
(<^>) :: Regex -> Regex -> Regex
(<^>) = Inter

-- | Synonym for the @Conc@ constructor
(***) :: Regex -> Regex -> Regex
(***) = Conc

-- * Instances

instance Show Regex where
        show Nil = "{}"
        show Eps = "ε"
        show (Sym c) = [c]
        show (Union r1 r2) = show r1 ++ "<|>" ++ show r2 
        show (Inter r1 r2) = show r1 ++ "<^>" ++ show r2 
        show (Conc r1 r2) = show r1 ++ "***" ++ show r2 
        show (Star r) = '(' : show r ++ ")*"

instance Semigroup Regex where
        (<>) = (***)

instance Monoid Regex where
        mempty = Nil


-- * Derivatives

-- | Defining the (Brzozowski) derivative of a language R 
-- with respect to the string \(u \in \Sigma^*\) to be
-- \[
--      u^{-1} L = \left\{ v : uv \in L \right\}
-- \]
derive :: Char -> Regex -> Regex
derive = undefined

