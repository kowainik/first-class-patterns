-----------------------------------------------------------------------------
-- |
-- Module:      Data.Pattern.Pattern
-- License:     BSD3
-- Maintainer:  Brent Yorgey <byorgey@cis.upenn.edu>
-- Stability:   experimental
-- Portability: non-portable (see .cabal)
--
-- The main types used in the implementation of first-class patterns:
-- 'Pattern' and 'Clause'.
-----------------------------------------------------------------------------

{-# LANGUAGE PolyKinds, DataKinds #-}
module Data.Pattern.Base (
  -- * Patterns
  Pattern(..),

  -- * Clauses
  Clause, runClause,
  (->>), (<|>),

  -- * Internals
  module Data.Pattern.Base.TypeList,
  module Data.Pattern.Base.Tuple,
 ) where

import Data.Pattern.Base.TypeList
import Data.Pattern.Base.Tuple

import Data.Maybe

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader

-- | The pattern type. A value of type @Pattern vars a@ is a pattern
--   which matches values of type @a@ and binds variables with types
--   given by the type-list @vars@.  For example, something of type
--
-- > Pattern (a :*: c :*: Nil) (a,b,c)
--
--   is a pattern which matches against a triple and binds values of
--   types @a@ and @c@. (A pattern of this type can be constructed as
--   @tup3 var __ var@.)
--
--   Many \"normal\" patterns can be conveniently defined using 'mk0',
--   'mk1', 'mk2', and so on.
newtype Pattern vars a = Pattern { runPattern :: a -> Maybe (Tuple vars) }

-- | Pattern-match clauses. Typically something of the form
--
--   @pattern '->>' function@
--
--   where the function takes one argument for each variable bound by
--   the pattern.
--
--   Clauses can be constructed with @('->>')@, run with 'tryMatch',
--   and manipulated by the 'Monad' and 'MonadPlus' instances. In
--   particular, the @('<|>')@ operator from the 'Alternative' class
--   is the way to list multiple cases in a pattern.
newtype Clause a r = Clause { runClause :: ReaderT a Maybe r
                              -- ^ Extract the underlying computation
                              -- constituting a 'Clause'. This
                              -- function is not intended to be used
                              -- directly; instead, see 'match',
                              -- 'tryMatch', 'mmatch', and 'elim' from
                              -- "Data.Pattern.Common".
                            }
    deriving(Functor,Applicative,Monad,Alternative,MonadPlus)

-- (<|>) is infix 3, so we make (->>) infix 4.
infix 4 ->>

-- | Construct a 'Clause' from a pattern and a function which takes
--   one argument for each variable bound by the pattern. For example,
--
-- > pair __ nothing     ->> 3
-- > pair var nothing    ->> \x -> x + 3
-- > pair var (just var) ->> \x y -> x + y + 3
(->>) :: Pattern vars a -> Fun vars r -> Clause a r
(Pattern p) ->> k = Clause (ReaderT $ fmap (flip runTuple k) . p)
