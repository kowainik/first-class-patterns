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

{-# LANGUAGE PolyKinds, DataKinds #-}
module Data.Pattern.Common (

  -- * Pattern combinators
  -- ** Basic patterns
  var, give, __, pfail, cst, (/\), (\/),
  view, (-->), tryView, (-?>),
  is,

  -- ** Computational patterns
  pfilter, pmap, pfoldr,

  -- * Running matches
  match, tryMatch, mmatch,
  elim,

  -- * Patterns for common data types
  -- ** Booleans
  true, false,
  -- ** Tuples
  -- $tuples
  unit, tup0, pair, tup2, tup3, tup4, tup5,
  -- ** @Maybe@
  nothing, just,
  -- ** @Either@
  left, right,
  -- ** Lists
  nil, cons,
  -- ** Numerics
  zero, suc,

  -- * Building your own patterns
  -- ** Smart constructors for patterns
  -- | Build patterns from a selector function.
  mk0, mk1, mk2, mk3, mk4, mk5,

 ) where

import Data.Pattern.Base

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader

import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Data.Maybe


------------------------------------------------------------
-- Basic patterns

-- XXX todo: add examples of each combinator!

-- | Variable pattern: always succeeds, and binds the value to a variable.
var :: Pattern '[a] a
var = Pattern (Just . oneT)

-- | @give b@ always succeeds, ignoring the matched value and
--   providing the value @b@ instead.  Useful in conjunction with
--   @('/\')@ for providing default values in cases that would otherwise
--   not bind any values.
give :: b -> Pattern '[b] a
give b = Pattern (const (Just $ oneT b))

-- | Wildcard pattern: always succeeds, binding no variables. (This is
--   written as two underscores.)
__ :: Pattern '[] a
__ = is (const True)

-- | Failure pattern: never succeeds.
pfail :: Pattern '[] a
pfail = is (const False)

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
is :: (a -> Bool) -> Pattern '[] a
is g = mk0 (\a -> if g a then Just () else Nothing)

-- | Constant pattern: test for equality to the given constant.
--
--   @cst x = is (==x)@.
cst :: (Eq a) => a -> Pattern '[] a
cst x = is (==x)

-- | Conjunctive (and) pattern: matches a value against two patterns,
--   and succeeds only if both succeed, binding variables from both.
--
-- @(/\\) = 'mk2' (\\a -> Just (a,a))@
(/\) :: Pattern vs1 a -> Pattern vs2 a -> Pattern (vs1 :++: vs2) a
(/\) = mk2 (\a -> Just (a,a))

-- | Disjunctive (or) pattern: matches a value against the first
--   pattern, or against the second pattern if the first one fails.
(\/) :: Pattern as a -> Pattern as a -> Pattern as a
(Pattern l) \/ (Pattern r) = Pattern (\a -> l a `mplus` r a)

-- | View pattern: do some computation, then pattern match on the
--   result.
view :: (a -> b) -> Pattern vs b -> Pattern vs a
view f = mk1 (Just . f)

-- ->> is infix 4, so this ought to have higher precedence
infix 5 -->

-- | Convenient infix synonym for 'view'.
(-->) :: (a -> b) -> Pattern vs b -> Pattern vs a
(-->) = view

-- | Partial view pattern: do some (possibly failing) computation,
--   then pattern match on the result if the computation is successful.
tryView :: (a -> Maybe b) -> Pattern vs b -> Pattern vs a
tryView = mk1

infix 5 -?>

-- | Convenient infix synonym for 'tryView'.
(-?>) :: (a -> Maybe b) -> Pattern vs b -> Pattern vs a
(-?>) = tryView


------------------------------------------------------------
-- Computational patterns

-- XXX use (Tup vs ': '[]) or something like that instead of (Map [] vs)?

-- | @pfilter p@ matches every element of a 'F.Foldable' data structure
--   against the pattern @p@, discarding elements that do not match.
--   From the matching elements, binds a list of values corresponding
--   to each pattern variable.
pfilter :: (Distribute vs, F.Foldable t) => Pattern vs a -> Pattern (Map [] vs) (t a)
pfilter (Pattern p) = Pattern $ Just . distribute . catMaybes . map p . F.toList

-- | @pmap p@ matches every element of a 'T.Traversable' data
--   structure against the pattern @p@.  The entire match fails if any
--   of the elements fail to match @p@.  If all the elements match,
--   binds a @t@-structure full of bound values corresponding to each
--   variable bound in @p@.
pmap :: (Distribute vs, T.Traversable t) => Pattern vs a -> Pattern (Map t vs) (t a)
pmap (Pattern p) = Pattern $ fmap distribute . T.traverse p

-- | @pfoldr p f b@ matches every element of a 'F.Foldable' data
--   structure against the pattern @p@, discarding elements that do
--   not match.  Folds over the bindings produced by the matching
--   elements to produce a summary value.
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
pfoldr :: (F.Foldable t, Functor t) => Pattern vs a -> (Fun vs (b -> b)) -> b -> Pattern '[b] (t a)
pfoldr (Pattern p) f b = Pattern $ Just . oneT . foldr (flip runTuple f) b . catMaybes . F.toList . fmap p


------------------------------------------------------------
-- Running matches

-- | \"Runs\" a 'Clause', by matching it against a value and returning
--   a result if it matches, or @Nothing@ if the match fails.
tryMatch :: a -> Clause a r -> Maybe r
tryMatch = flip (runReaderT.runClause)

-- | 'match' satisfies the identity @match a c = fromJust (tryMatch a c)@.
match :: a -> Clause a r -> r
match = (fmap.fmap) (fromMaybe $ error "failed match") tryMatch

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


------------------------------------------------------------
-- Boolean patterns

-- | Match @True@.
true :: Pattern '[] Bool
true = is id

-- | Match @False@.
false :: Pattern '[] Bool
false = is not   -- is too!


------------------------------------------------------------
-- Tuple patterns

-- $tuples
--
-- If you need to pattern match on tuples bigger than 5-tuples, you
-- are Doing It Wrong.

-- | A strict match on the unit value @()@.
unit :: Pattern '[] ()
unit = mk0 (\() -> Just ())

-- | A synonym for 'unit'.
tup0 :: Pattern '[] ()
tup0 = unit

-- | Construct a pattern match against a pair from a pair of patterns.
pair :: Pattern vs1 a -> Pattern vs2 b -> Pattern (vs1 :++: vs2) (a,b)
pair (Pattern pa) (Pattern pb) = Pattern (\(a,b) -> (<>) <$> pa a <*> pb b)

-- | A synonym for 'pair'.
tup2 :: Pattern vs1 a -> Pattern vs2 b -> Pattern (vs1 :++: vs2) (a,b)
tup2 = pair

-- | Match a 3-tuple.
tup3 :: Pattern vs1 a ->
        Pattern vs2 b ->
        Pattern vs3 c ->
        Pattern (vs1 :++: vs2 :++: vs3) (a,b,c)
tup3 (Pattern pa) (Pattern pb) (Pattern pc) =
   Pattern (\(a,b,c) -> (<>) <$> pa a <*> ((<>) <$> pb b <*> pc c))

-- | Match a 4-tuple.
tup4 :: Pattern vs1 a ->
        Pattern vs2 b ->
        Pattern vs3 c ->
        Pattern vs4 d ->
        Pattern (vs1 :++: vs2 :++: vs3 :++: vs4) (a,b,c,d)
tup4 (Pattern pa) (Pattern pb) (Pattern pc) (Pattern pd) =
   Pattern (\(a,b,c,d) -> (<>) <$> pa a <*> ((<>) <$> pb b <*> ((<>) <$> pc c <*> pd d)))

-- | Match a 5-tuple.
tup5 :: Pattern vs1 a ->
        Pattern vs2 b ->
        Pattern vs3 c ->
        Pattern vs4 d ->
        Pattern vs5 e ->
        Pattern (vs1 :++: vs2 :++: vs3 :++: vs4 :++: vs5) (a,b,c,d,e)
tup5 (Pattern pa) (Pattern pb) (Pattern pc) (Pattern pd) (Pattern pe) =
   Pattern (\(a,b,c,d,e) -> (<>) <$> pa a <*> ((<>) <$> pb b <*> ((<>) <$> pc c <*> ((<>) <$> pd d <*> pe e))))


------------------------------------------------------------
-- Maybe

-- | Match the 'Nothing' constructor of 'Maybe'.
nothing :: Pattern '[] (Maybe a)
nothing = is isNothing

-- | Match the 'Just' constructor of 'Maybe'.
just :: Pattern vs a -> Pattern vs (Maybe a)
just = mk1 id


------------------------------------------------------------
-- Either

-- | Match the 'Left' constructor of 'Either'.
left :: Pattern vs a -> Pattern vs (Either a b)
left = mk1 (either Just (const Nothing))

-- | Match the 'Right' constructor of 'Either'.
right :: Pattern vs b -> Pattern vs (Either a b)
right = mk1 (either (const Nothing) Just)


------------------------------------------------------------
-- Lists

-- | Match the empty list.
nil :: Pattern '[] [a]
nil = is null

-- | Match a cons.
cons :: Pattern vs1 a -> Pattern vs2 [a] -> Pattern (vs1 :++: vs2) [a]
cons = mk2 (\l -> case l of { (x:xs) -> Just (x,xs); _ -> Nothing })


------------------------------------------------------------
-- Numerics

-- | Match zero.
zero :: (Integral a, Eq a) => Pattern '[] a
zero = cst 0

-- | Match a natural number which is the successor of another natural
--   (and match the predecessor with a nested pattern).  Together,
--   'zero' and 'suc' allow viewing @Integral@ types as Peano numbers.
--
--   Note that 'suc' never matches negative numbers.
suc :: (Integral a, Eq a) => Pattern vs a -> Pattern vs a
suc = mk1 (\n -> if (n <= 0) then Nothing else Just (n-1))


-- XXX better names? and export
twice :: (Integral a, Eq a) => Pattern vs a -> Pattern vs a
twice = mk1 (\n -> if even n then Just (n `div` 2) else Nothing)

succtwice :: (Integral a, Eq a) => Pattern vs a -> Pattern vs a
succtwice = mk1 (\n -> if odd n then Just (n `div` 2) else Nothing)



------------------------------------------------------------
-- Constructing patterns

mk0 :: (a -> Maybe ()) -> Pattern '[] a
mk0 g = Pattern (fmap (const zeroT) . g)

mk1 :: (a -> Maybe b) -> Pattern vs b -> Pattern vs a
mk1 g (Pattern p) = Pattern (\a -> g a >>= p)

mk2 :: (a -> Maybe (b,c)) ->
       Pattern vs1 b ->
       Pattern vs2 c ->
       Pattern (vs1 :++: vs2) a
mk2 g b c = mk1 g (pair b c)

mk3 :: (a -> Maybe (b,c,d)) ->
       Pattern vs1 b ->
       Pattern vs2 c ->
       Pattern vs3 d ->
       Pattern (vs1 :++: vs2 :++: vs3) a
mk3 g b c d = mk1 g (tup3 b c d)

mk4 :: (a -> Maybe (b,c,d,e)) ->
       Pattern vs1 b ->
       Pattern vs2 c ->
       Pattern vs3 d ->
       Pattern vs4 e ->
       Pattern (vs1 :++: vs2 :++: vs3 :++: vs4) a
mk4 g b c d e = mk1 g (tup4 b c d e)

mk5 :: (a -> Maybe (b,c,d,e,f)) ->
       Pattern vs1 b ->
       Pattern vs2 c ->
       Pattern vs3 d ->
       Pattern vs4 e ->
       Pattern vs5 f ->
       Pattern (vs1 :++: vs2 :++: vs3 :++: vs4 :++: vs5) a
mk5 g b c d e f = mk1 g (tup5 b c d e f)



-- XXX de Bruijn references for nonlinear patterns?
{-
data Ref :: * -> * -> *
  RZero :: Ref (h ': t) h
  RSucc :: Ref t a -> Ref (h ': t) a

-- Can't implement this with the current definition of Pattern --
-- there is no way to access previously matched values.  Plus the type
-- will be a problem: can't infer the type xs that the reference is
-- indexing into, since the reference itself doesn't bind any
-- variables.
--
-- Essentially what it boils down to is that this pattern is rather
-- non-compositional. =(
ref :: Ref xs a -> Pattern '[] a
ref = undefined
-}