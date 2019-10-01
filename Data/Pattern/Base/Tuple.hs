-----------------------------------------------------------------------------
-- |
-- Module:      Data.Pattern.Base.Tuple
-- License:     BSD3
-- Maintainer:  Brent Yorgey <byorgey@cis.upenn.edu>
-- Stability:   experimental
-- Portability: non-portable (see .cabal)
--
-- Various types defined inductively as type families or data families
-- on type-lists.
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Data.Pattern.Base.Tuple (
  -- * Functions
  Fun,
  -- * Tuples
  Tuple,
  zeroT,
  oneT,
  (<+>),
  runTuple,
  -- * Mapping and distributing over tuples
  Map, Distribute(..)
 ) where

import Data.Pattern.Base.Difference
import Data.Pattern.Base.TypeList
import Data.Kind (Type)

-- | Curried functions. We have
--
-- @Fun '[x1, ..., xn] r   =   x1 -> ... -> xn -> r@
type family   Fun (xs :: [Type]) r
type instance Fun '[]      r = r
type instance Fun (h ': t) r = h -> Fun t r

data family   Tup (xs :: [Type])
data instance Tup '[]      = Unit
data instance Tup (h ': t) = Pair h (Tup t)

class Uncurriable xs where
  uncurryT :: (Tup xs -> r) -> Fun xs r

instance Uncurriable '[] where
  uncurryT f = f Unit

instance Uncurriable t => Uncurriable (h ': t) where
  uncurryT f = \h -> uncurryT (\tup -> f (Pair h tup))

newtype Tuple' xs = Tuple' { runTuple' :: forall r. Fun xs r -> r }

-- | Tuples with types given by @xs@.
newtype Tuple xs = Tuple (D Tuple' xs)

-- | The empty tuple
zeroT :: Tuple '[]
zeroT = Tuple zeroD

-- | The singleton tuple
oneT :: a -> Tuple '[a]
oneT a = Tuple (mkOneD (\(Tuple' t) -> Tuple' (\k -> t (k a))))

-- XXX somehow derive this from a general 'TypeList' class?  and also Uncurriable?
class Tupable xs where
  mkTuple :: Tup xs -> Tuple xs

instance Tupable '[] where
  mkTuple Unit = zeroT

instance Tupable t => Tupable (h ': t) where
  mkTuple (Pair h t) = oneT h <+> mkTuple t

-- | Concatenation of tuples.
(<+>) :: Tuple xs -> Tuple ys -> Tuple (xs :++: ys)
Tuple xs <+> Tuple ys = Tuple (xs `plusD` ys)

-- | Runs a tuple by applying it to a curried function.
runTuple :: Tuple xs -> Fun xs r -> r
runTuple (Tuple t) = runTuple' (evalD (Tuple' id) t)

-- | Runs a tuple by applying it to an uncurried function expecting
--   nested pairs.
runTupleT :: Uncurriable xs => Tuple xs -> (Tup xs -> r) -> r
runTupleT t f = runTuple t (uncurryT f)

unconsTuple :: (Uncurriable t, Tupable t) => Tuple (h ': t) -> (h, Tuple t)
unconsTuple t = runTupleT t (\(Pair h t) -> (h, mkTuple t))

tupleHead :: (Uncurriable t, Tupable t) => Tuple (h ': t) -> h
tupleHead = fst . unconsTuple

tupleTail :: (Uncurriable t, Tupable t) => Tuple (h ': t) -> Tuple t
tupleTail = snd . unconsTuple

type family Map (f :: Type -> Type) (xs :: [Type]) :: [Type]
type instance Map f '[]      = '[]
type instance Map f (h ': t) = f h ': Map f t

class Distribute xs where
  distribute :: Functor f => f (Tuple xs) -> Tuple (Map f xs)

instance Distribute '[] where
  distribute _ = zeroT

instance (Uncurriable t, Tupable t, Distribute t) => Distribute (h ': t) where
--  distribute :: f (Tuple (h :*: t)) -> Tuple (f h :*: Map f t)
  distribute f = oneT (fmap tupleHead f) <+> distribute (fmap tupleTail f)
