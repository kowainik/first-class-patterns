-----------------------------------------------------------------------------
-- |
-- Module:      Data.Pattern.Common
-- License:     BSD3
-- Maintainer:  Brent Yorgey <byorgey@cis.upenn.edu>
-- Stability:   experimental
-- Portability: non-portable (see .cabal)
--
-- A collection of useful pattern combinators.
-----------------------------------------------------------------------------

module Data.Pattern.Common (

  -- * Pattern combinators
  -- ** Basic patterns
  var, give, (/\), (\/), view, tryView,
  -- ** Non-binding patterns
  is, cst, __, pfail,
  -- ** Computational patterns
  pfilter, pmap, pfoldr,

  -- * Running matches
  -- ** Anonymous matching
  elim,
  -- ** Monadic matching
  mmatch,

  -- * Provided patterns
  -- ** Tuple patterns
  tup0, tup1, tup2, pair, tup3, tup4, tup5,
  -- ** @Maybe@ patterns
  nothing, just,
  -- ** @Either@ patterns
  left, right,
  -- ** List patterns
  nil, cons,
  -- ** Numeric patterns
  zero, suc,

  -- * Building your own patterns
  -- ** Smart constructors for patterns
  -- | Build patterns from a selector function.
  mk0, mk1, mk2, mk3, mk4, mk5,

 ) where

import Data.Pattern.Base

import Control.Applicative
import Control.Monad
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Data.Maybe

-- XXX todo: add examples of each combinator!

-- | Variable pattern: always succeeds, and binds the value to a variable.
var :: Pattern (a :*: Nil) a
var = Pattern (Just . oneT)

-- | Wildcard pattern: always succeeds, binding no variables. (This is
--   written as two underscores.)
__ :: Pat0 a
__ = is (const True)

-- | @give b@ always succeeds, ignoring the matched value and
--   providing the value @b@ instead.  Useful in conjunction with
--   @(/\\)@ for providing default values in cases that would otherwise
--   not bind any values.
give :: b -> Pattern (b :*: Nil) a
give b = Pattern (const (Just $ oneT b))

-- | Failure pattern: never succeeds.
pfail :: Pat0 a
pfail = is (const False)


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
--   (Note that 'tryView' is a synonym for 'mk1'.)
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

-- | Predicate pattern. Succeeds if the given predicate yields 'True',
--   fails otherwise.
--
--   Can be used with @('/\')@ for some uses similar to pattern guards:
--
-- > match a $
-- >      left (var /\ is even) ->> id
-- >  <|> left  __              ->> const 0
-- >  <|> right __              ->> const 1
--
-- Note that 'is' is like 'mk0' but with 'Bool' instead of @'Maybe'
-- ()@.
is :: (a -> Bool) -> Pat0 a
is g = mk0 (\a -> if g a then Just () else Nothing)

-- | Constant pattern: test for equality to the given constant.
--
--   @cst x = is (==x)@.
cst :: (Eq a) => a -> Pat0 a
cst x = is (==x)

-- | Match the 'Left' constructor of 'Either'.
left :: Pat1 a (Either a b)
left = mk1 (either Just (const Nothing))

-- | Match the 'Right' constructor of 'Either'.
right :: Pat1 b (Either a b)
right = mk1 (either (const Nothing) Just)

-- | Match the 'Nothing' constructor of 'Maybe'.
nothing :: Pat0 (Maybe a)
nothing = is isNothing

-- | Match the 'Just' constructor of 'Maybe'.
just :: Pat1 a (Maybe a)
just = mk1 id

-- | Match the empty list.
nil :: Pat0 [a]
nil = is null

-- | Match a cons.
cons :: Pat2 a [a] [a]
cons = mk2 (\l -> case l of { (x:xs) -> Just (x,xs); _ -> Nothing })


-- XXX use (Tup vs :*: Nil) or something like that instead of (Map [] vs)?

-- | @pfilter p@ matches every element of a 'F.Foldable' data structure
--   against the pattern @p@, discarding elements that do not match.
--   From the matching elements, binds a list of values corresponding
--   to each pattern variable.
--
--   For example, XXX
--
pfilter :: (Distribute vs, F.Foldable t) => Pattern vs a -> Pattern (Map [] vs) (t a)
pfilter (Pattern p) = Pattern $ Just . distribute . catMaybes . map p . F.toList

-- | @pmap p@ matches every element of a 'T.Traversable' data
--   structure against the pattern @p@.  The entire match fails if any
--   of the elements fail to match @p@.  If all the elements match,
--   binds a @t@-structure full of bound values corresponding to each
--   variable bound in @p@.
--
--   For example, XXX
pmap :: (Distribute vs, T.Traversable t) => Pattern vs a -> Pattern (Map t vs) (t a)
pmap (Pattern p) = Pattern $ fmap distribute . T.traverse p

-- | @pfoldr p f b@ matches every element of a 'F.Foldable' data
--   structure against the pattern @p@, discarding elements that do
--   not match.  Folds over the bindings produced by the matching
--   elements to produce a summary value.
--
--   For example,
--
--   The same functionality could be achieved by matching with
--   @pfilter p@ and then appropriately combining and folding the
--   resulting lists of bound values.  In particular, if @p@ binds
--   only one value we have
--
-- > match t (pfoldr p f b ->> id) === match t (pfilter p ->> foldr f b)
--
--   However, when @p@ binds more than one value, it can be convenient
--   to be able to process the bindings from each match together,
--   rather than having to deal with them once they are separated out
--   into separate lists.
pfoldr :: (F.Foldable t, Functor t) => Pattern vs a -> (Fun vs (b -> b)) -> b -> Pattern (b :*: Nil) (t a)
pfoldr (Pattern p) f b = Pattern $ Just . oneT . foldr (flip runTuple f) b . catMaybes . F.toList . fmap p

-- | \"0-tuple pattern\". A strict match on the @()@.
tup0 :: Pat0 ()
tup0 = mk0 (\() -> Just ())

-- | \"1-tuple pattern\". Rather useless.
tup1 :: Pat1 a a
tup1 = mk1 Just

-- | \"2-tuple pattern\"
tup2 :: Pat2 a b (a,b)
tup2 (Pattern pa) (Pattern pb) = Pattern (\(a,b) -> (<>) <$> pa a <*> pb b)

-- | Convenient synonym for 'tup2'.
pair :: Pat2 a b (a,b)
pair = tup2

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
mk0 g = Pattern (fmap (const zeroT) . g)

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


zero :: (Integral a, Eq a) => Pat0 a
zero = cst 0

suc :: (Integral a, Eq a) => Pat1 a a
suc = mk1 (\n -> if (n <= 0) then Nothing else Just (n-1))


-- XXX better names? and export
twice :: (Integral a, Eq a) => Pat1 a a
twice = mk1 (\n -> if even n then Just (n `div` 2) else Nothing)

succtwice :: (Integral a, Eq a) => Pat1 a a
succtwice = mk1 (\n -> if odd n then Just (n `div` 2) else Nothing)


-- XXX de Bruijn references for nonlinear patterns?
{-
data Ref :: * -> * -> *
  RZero :: Ref (h :*: t) h
  RSucc :: Ref t a -> Ref (h :*: t) a

-- Can't implement this with the current definition of Pattern --
-- there is no way to access previously matched values.  Plus the type
-- will be a problem: can't infer the type xs that the reference is
-- indexing into, since the reference itself doesn't bind any
-- variables.
--
-- Essentially what it boils down to is that this pattern is rather
-- non-compositional. =(
ref :: Ref xs a -> Pattern Nil a
ref = undefined
-}