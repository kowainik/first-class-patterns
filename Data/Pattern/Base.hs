-----------------------------------------------------------------------------
-- |
-- Module:      Data.Pattern.Pattern
-- License:     BSD3
-- Maintainer:  Reiner Pope <reiner.pope@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- The main types used.
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
  (->>), tryMatch, match,
  module Data.Pattern.Base.TypeList,
  module Data.Pattern.Base.Tuple,
 ) where

import Data.Pattern.Base.TypeList
import Data.Pattern.Base.Tuple

import Data.Maybe

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader


-- | The pattern type. A @Pattern vars a@ is a pattern which matches
-- against @a@s and binds variables with types given by the type-list
-- @vars@. 
-- 
-- Although this is the basic type used by patterns, many of
-- pattern combinators (for instance, 'Data.Pattern.Base.Common.left')
-- have types better expressed by the type synonyms 'Pat0', 'Pat1',
-- 'Pat2', etc, 'Pat5', so that nesting of patterns (e.g. @left (tup2
-- var var)@) can be written as function application. 
--
-- Most \"normal\" pattern matchers (in fact, all of the matchers in
-- "Data.Pattern.Common" except @var@ and @(\/)@) can be conveniently
-- defined using @mk0@, @mk1@, etc, @mk5@.
newtype Pattern vars a = Pattern { runPattern :: a -> Maybe (Tuple vars) }

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

{-newtype Clause a r = Clause { runClause :: a -> Maybe r }

instance Functor (Clause a) where
    {-# INLINE fmap #-}
    fmap = liftM

instance Applicative (Clause a) where
    {-# INLINE pure #-}
    pure = return
    {-# INLINE (<*>) #-}
    (<*>) = ap

instance Monad (Clause a) where
    {-# INLINE return #-}
    return = Clause . return . return
    {-# INLINE (>>=) #-}
    (Clause m) >>= k = Clause (\a -> m a >>= (\r -> runClause (k r) a))

instance Alternative (Clause a) where
    {-# INLINE empty #-}
    empty = Clause (pure empty)
    {-# INLINE (<|>) #-}
    Clause l <|> Clause r = Clause (\a -> l a <|> r a)
-}
-- (<|>) has infix 3, so we make (->>) infix 4.
infix 4 ->>

-- | Constructs a 'Clause'.
(->>) :: Pattern vars a -> Fun vars r -> Clause a r
(Pattern p) ->> k = Clause (ReaderT $ \a -> fmap (\f -> runTuple f k) (p a))


-- | \"Runs\" a 'Clause'.
tryMatch :: a -> Clause a r -> Maybe r
tryMatch = flip (runReaderT.runClause)

-- | @match a c = fromJust (tryMatch a c)@
match :: a -> Clause a r -> r
match = (fmap.fmap) (maybe (error "match") id) tryMatch
