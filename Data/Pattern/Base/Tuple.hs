-----------------------------------------------------------------------------
-- |
-- Module:      Data.Pattern.Base.Tuple
-- License:     BSD3
-- Maintainer:  Reiner Pope <reiner.pope@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Various types defined inductively as type families or data families
-- on type-lists.
-----------------------------------------------------------------------------


module Data.Pattern.Base.Tuple (
  -- * Functions
  Fun,
  -- * Tuples
  Tuple,
  zero,
  one,
  (<>),
  runTuple,
 ) where

import Data.Pattern.Base.TypeList
import Data.Pattern.Base.Difference

-- | Curried functions. We have
-- 
-- @Fun (x1 :*: x2 :*: ... :*: xn :*: Nil) r ~ x1 -> x2 -> ... -> xn -> r@
type family   Fun xs r
type instance Fun Nil     r = r
type instance Fun (h:*:t) r = h -> Fun t r

newtype Tuple' xs = Tuple' { runTuple' :: forall r. Fun xs r -> r }

-- | Tuples with types given by @xs@.
newtype Tuple xs = Tuple (D Tuple' xs)

-- | The empty tuple
zero :: Tuple Nil
zero = Tuple zeroD

-- | The singleton tuple
one :: a -> Tuple (a :*: Nil)
one a = Tuple (mkOneD (\(Tuple' t) -> Tuple' (\k -> t (k a))))

-- | Concatenation of tuples.
(<>) :: Tuple xs -> Tuple ys -> Tuple (xs :++: ys)
(Tuple xs) <> (Tuple ys) = Tuple (xs `plusD` ys)

-- | Runs a tuple by applying it to a function.
runTuple :: Tuple xs -> Fun xs r -> r
runTuple (Tuple t) = runTuple' (evalD (Tuple' id) t)

