-----------------------------------------------------------------------------
-- |
-- Module:      Data.Pattern.TypeList.Difference
-- License:     BSD3
-- Maintainer:  Brent Yorgey <byorgey@cis.upenn.edu>
-- Stability:   experimental
-- Portability: non-portable (see .cabal)
--
-- \"Difference list converters\". Developed in <http://reinerp.wordpress.com/2009/07/18/difference-type-list/>
-----------------------------------------------------------------------------

{-# LANGUAGE PolyKinds, DataKinds #-}
module Data.Pattern.Base.Difference (
  Difference(..),
  D,
 ) where

import Data.Pattern.Base.TypeList

import Unsafe.Coerce

-- | The API presented by @Data.Pattern.Base.Difference.GADT@ and
-- @Data.Pattern.Base.Difference.Coerce@. An instance of 'Difference' is a
-- type which converts an inductively-defined type to one with
-- an efficient append operation.
class Difference d where
    -- | constructs the empty @d t@.
    zeroD :: d t '[]
    -- | appends two @d t@s.
    plusD :: d t xs -> d t ys -> d t (xs :++: ys)
    -- | given a \"cons\" operation, constructs the singleton @d t@.
    mkOneD :: (forall ys. t ys -> t (a ': ys)) -> d t '[a]
    -- | given a \"nil\" value, \"runs\" the @d t@.
    evalD :: t '[] -> d t xs -> t xs

newtype D t xs = D (CoerceD t xs) deriving(Difference)

----- GADT implementation (pure (no cheating), recursive) -------------
data Proxy a
proxy :: forall (a :: [*]). Proxy a
proxy = undefined

data GadtD t xs = List xs => GadtD (forall ys. t ys -> t (xs :++: ys))

instance Difference GadtD where
    {-# INLINE zeroD #-}
    zeroD = GadtD id
    {-# INLINE plusD #-}
    plusD (GadtD fx :: GadtD t xs) (GadtD fy :: GadtD t ys) =
        case closure (proxy :: Proxy xs) (proxy :: Proxy ys) of
          ListD -> GadtD (\(zs :: t zs) ->
            case assoc (proxy :: Proxy xs) (proxy :: Proxy ys) (proxy :: Proxy zs) of
              Equal -> fx (fy zs))
    {-# INLINE mkOneD #-}
    mkOneD f = GadtD f
    {-# INLINE evalD #-}
    evalD nil (GadtD f :: GadtD t xs) =
        case rightIdent (proxy :: Proxy xs) of
          Equal -> f nil

class List a where
    closure :: forall b. List b =>
       Proxy a -> Proxy b ->
        ListD (a :++: b)
    assoc :: forall b c.
       Proxy a -> Proxy b -> Proxy c ->
        ((a :++: (b :++: c)) :==: ((a :++: b) :++: c))
    rightIdent :: Proxy a ->
        (a :++: '[]) :==: a

instance List '[] where
    {-# INLINE closure #-}
    closure _ _  = ListD
    {-# INLINE assoc #-}
    assoc _ _ _  = Equal
    {-# INLINE rightIdent #-}
    rightIdent _ = Equal

instance List t => List (h ': t) where
    {-# INLINE closure #-}
    closure _ b  = case closure (proxy :: Proxy t) b of
                     ListD -> ListD
    {-# INLINE assoc #-}
    assoc _ b c  = case assoc (proxy :: Proxy t) b c of
                     Equal -> Equal
    {-# INLINE rightIdent #-}
    rightIdent _ = case rightIdent (proxy :: Proxy t) of
                     Equal -> Equal

data a :==: b where
    Equal :: forall (a :: [*]). a :==: a
data ListD a where
    ListD :: List a => ListD a


----- UnsafeCoerce implementation (cheating, nonrecursive) ----------------
newtype CoerceD t xs = CoerceD (forall ys. t ys -> t (xs :++: ys))

instance Difference CoerceD where
    zeroD = CoerceD id
    plusD (CoerceD fx :: CoerceD t xs) (CoerceD fy :: CoerceD t ys) =
        CoerceD (\(zs :: t zs) ->
            case assoc2 (proxy :: Proxy xs) (proxy :: Proxy ys) (proxy :: Proxy zs) of
              Equal -> fx (fy zs))
    mkOneD f = CoerceD f
    evalD nil (CoerceD f :: CoerceD t xs) =
        case rightIdent2 (proxy :: Proxy xs) of
          Equal -> f nil

assoc2 :: Proxy a -> Proxy b -> Proxy c -> (a :++: (b :++: c)) :==: ((a :++: b) :++: c)
assoc2 _ _ _ = unsafeCoerce Equal

rightIdent2 :: Proxy a -> (a :++: '[]) :==: a
rightIdent2 _ = unsafeCoerce Equal

