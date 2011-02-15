-----------------------------------------------------------------------------
-- |
-- Module:      Data.Pattern.Pattern
-- License:     BSD3
-- Maintainer:  Brent Yorgey <byorgey@cis.upenn.edu>
-- Stability:   experimental
-- Portability: non-portable (see .cabal)
--
-- The main types used in the implementation of first-class patterns.
-----------------------------------------------------------------------------


module Data.Pattern.Base (
  -- * Patterns
  Pattern(..),
  -- | Pattern synonyms. A @PatN@ is a function which takes @N@
  -- subpatterns and yields a 'Pattern' which binds all of the
  -- subpatterns' variables in order.
  Pat0, Pat1, Pat2, Pat3, Pat4, Pat5,
  -- * Clauses
  Clause,
  (->>), (<|>), tryMatch, match,
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

-- XXX do away with these?
type Pat0 a = Pattern Nil a
type Pat1 b a = forall bs. Pattern bs b -> Pattern bs a
type Pat2 b c a = forall bs cs. Pattern bs b -> Pattern cs c -> Pattern (bs :++: cs) a
type Pat3 b c d a = forall bs cs ds. Pattern bs b -> Pattern cs c -> Pattern ds d -> Pattern (bs :++: cs :++: ds) a
type Pat4 b c d e a = forall bs cs ds es. Pattern bs b -> Pattern cs c -> Pattern ds d -> Pattern es e -> Pattern (bs :++: cs :++: ds :++: es) a
type Pat5 b c d e f a = forall bs cs ds es fs. Pattern bs b -> Pattern cs c -> Pattern ds d -> Pattern es e -> Pattern fs f -> Pattern (bs :++: cs :++: ds :++: es :++: fs) a


-- | Pattern-match clauses. Typically something of the form
--
-- @pattern '->>' function@
--
-- Clauses can be constructed with @('->>')@, run with 'tryMatch', and
-- manipulated by the 'Monad' and 'MonadPlus' instances. In
-- particular, @('<|>')@ of the 'Alternative' class is the way to list
-- multiple cases in a pattern.
newtype Clause a r = Clause { runClause :: ReaderT a Maybe r }
    deriving(Functor,Applicative,Monad,Alternative,MonadPlus)

-- (<|>) has infix 3, so we make (->>) infix 4.
infix 4 ->>

-- | Constructs a 'Clause'.
(->>) :: Pattern vars a -> Fun vars r -> Clause a r
(Pattern p) ->> k = Clause (ReaderT $ \a -> fmap (\f -> runTuple f k) (p a))


-- | \"Runs\" a 'Clause', by matching it against a value and returning
--   a result if it matches, or @Nothing@ if the match fails.
tryMatch :: a -> Clause a r -> Maybe r
tryMatch = flip (runReaderT.runClause)

-- | 'match' satisfies the equation @match a c = fromJust (tryMatch a c)@.
match :: a -> Clause a r -> r
match = (fmap.fmap) (fromMaybe $ error "failed match") tryMatch
