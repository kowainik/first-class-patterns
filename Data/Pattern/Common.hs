-----------------------------------------------------------------------------
-- |
-- Module:      Data.Pattern.Common
-- License:     BSD3
-- Maintainer:  Reiner Pope <reiner.pope@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Common pattern combinators.
-----------------------------------------------------------------------------


module Data.Pattern.Common (
  -- * Basic patterns
  var, __, failp, (/\), (\/), view, tryView,
  -- * Non-binding patterns
  is, cst,
  -- * Anonymous matching
  elim,
  -- * Monadic matching
  mmatch,
  -- * Smart constructors for patterns
  -- | Build patterns from a selector function.
  mk0, mk1, mk2, mk3, mk4, mk5,
  -- * Tuple patterns
  tup0, tup1, tup2, tup3, tup4, tup5,
  -- * @Either@ patterns
  left, right,
  -- * List patterns
  nil, cons,
 ) where

import Data.Pattern.Base

import Control.Applicative
import Control.Monad

import Data.Maybe

-- XXX todo: add examples of each combinator!

-- | Variable pattern: always succeeds, and binds the value to a variable.
var :: Pattern (a :*: Nil) a
var = Pattern (Just . one)

-- | Wildcard pattern: always succeeds. (This is written as two underscores.)
__ :: Pat0 a
__ = is (const True)

-- | Failure pattern: never succeeds.
failp :: Pat0 a
failp = is (const False)

-- | Conjunctive (and) pattern: matches a value against two patterns,
--   and succeeds only if both succeed, binding variables from both.
--
-- @(/\\) = 'mk2' (\\a -> Just (a,a))@
(/\) :: Pat2 a a a
(/\) = mk2 (\a -> Just (a,a))

-- | Disjunctive (or) pattern: matches a value against the first
--   pattern, or against the second pattern if the first one fails.
(\/) :: Pattern as a -> Pattern as a -> Pattern as a
(Pattern l) \/ (Pattern r) = Pattern (\a -> l a `mplus` r a)

-- | View pattern: do some computation, then pattern match on the
--   result.
view :: (a -> b) -> Pat1 b a
view f = mk1 (Just . f)

-- | Partial view pattern: do some (possibly failing) computation,
--   then pattern match on the result if the computation is successful.
--   Note that 'tryView' is a synonym for 'mk1'.
tryView :: (a -> Maybe b) -> Pat1 b a
tryView = mk1

-- | @elim = flip 'match'@
--
-- Useful for anonymous matching (or for building \"eliminators\",
-- like 'maybe' and 'either'). For example:
--
-- > either withLeft withRight = elim $
-- >              left  var ->> withLeft
-- >          <|> right var ->> withRight
elim :: Clause a r -> a -> r
elim = flip match

-- | @mmatch m p = m >>= 'elim' p@
--
-- Useful for applicative-looking monadic pattern matching, as in
--
-- > ex7 :: IO ()
-- > ex7 = mmatch getLine $
-- >       cst "" ->> return ()
-- >   <|> var    ->> putStrLn . ("You said " ++)
mmatch :: (Monad m) => m a -> Clause a (m b) -> m b
mmatch m p = m >>= elim p

-- | \"Predicate pattern\". 'mk0' but with 'Bool' instead of @'Maybe' ()@.
-- Succeeds if function yields 'True', fails otherwise.
--
-- Can be used with @('/\')@ for some uses similar to pattern guards:
--
-- @match a $
--      left (var /\\ is even) ->> id
--  ||| left __               ->> const 0
--  ||| right __              ->> const 1@
is :: (a -> Bool) -> Pat0 a
is g = mk0 (\a -> if g a then Just () else Nothing)

-- | \"Constant patterns\": tests for equality to the given constant.
-- @cst x = is (==x)@
cst :: (Eq a) => a -> Pat0 a
cst x = is (==x)

-- | Matches the 'Left' of an 'Either'.
left :: Pat1 a (Either a b)
left = mk1 (either Just (const Nothing))

-- | Matches the 'Right' of an 'Either'.
right :: Pat1 b (Either a b)
right = mk1 (either (const Nothing) Just)

mk2 :: (a -> Maybe (b,c)) -> Pat2 b c a
mk2 g b c = mk1 g (tup2 b c)

mk3 :: (a -> Maybe (b,c,d)) -> Pat3 b c d a
mk3 g b c d = mk1 g (tup3 b c d)

-- | Matches the empty list.
nil :: Pat0 [a]
nil = is null

-- | Matches a cons.
cons :: Pat2 a [a] [a]
cons = mk2 (\l -> case l of { (x:xs) -> Just (x,xs); _ -> Nothing })


-- | \"0-tuple pattern\". A strict match on the @()@.
tup0 :: Pat0 ()
tup0 = mk0 (\() -> Just ())

-- | \"1-tuple pattern\". Rather useless.
tup1 :: Pat1 a a
tup1 = mk1 Just

-- | \"2-tuple pattern\"
tup2 :: Pat2 a b (a,b)
tup2 (Pattern pa) (Pattern pb) = Pattern (\(a,b) -> (<>) <$> pa a <*> pb b)

-- | \"3-tuple pattern\"
tup3 :: Pat3 a b c (a,b,c)
tup3 (Pattern pa) (Pattern pb) (Pattern pc) =
   Pattern (\(a,b,c) -> (<>) <$> pa a <*> ((<>) <$> pb b <*> pc c))

-- | \"4-tuple pattern\"
tup4 :: Pat4 a b c d (a,b,c,d)
tup4 (Pattern pa) (Pattern pb) (Pattern pc) (Pattern pd) =
   Pattern (\(a,b,c,d) -> (<>) <$> pa a <*> ((<>) <$> pb b <*> ((<>) <$> pc c <*> pd d)))

-- | \"5-tuple pattern\"
tup5 :: Pat5 a b c d e (a,b,c,d,e)
tup5 (Pattern pa) (Pattern pb) (Pattern pc) (Pattern pd) (Pattern pe) =
   Pattern (\(a,b,c,d,e) -> (<>) <$> pa a <*> ((<>) <$> pb b <*> ((<>) <$> pc c <*> ((<>) <$> pd d <*> pe e))))


mk0 :: (a -> Maybe ()) -> Pat0 a
mk0 g = Pattern (fmap (const zero) . g)

mk1 :: (a -> Maybe b) -> Pat1 b a
mk1 g (Pattern p) = Pattern (\a -> g a >>= p)

mk2 :: (a -> Maybe (b,c)) -> Pat2 b c a
mk2 g b c = mk1 g (tup2 b c)

mk3 :: (a -> Maybe (b,c,d)) -> Pat3 b c d a
mk3 g b c d = mk1 g (tup3 b c d)

mk4 :: (a -> Maybe (b,c,d,e)) -> Pat4 b c d e a
mk4 g b c d e = mk1 g (tup4 b c d e)

mk5 :: (a -> Maybe (b,c,d,e,f)) -> Pat5 b c d e f a
mk5 g b c d e f = mk1 g (tup5 b c d e f)
