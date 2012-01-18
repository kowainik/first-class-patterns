-----------------------------------------------------------------------------
-- |
-- Module:      Data.Pattern.Base.TypeList
-- License:     BSD3
-- Maintainer:  Brent Yorgey <byorgey@cis.upenn.edu>
-- Stability:   experimental
-- Portability: non-portable (see .cabal)
--
-- Type-level lists. These lists only describe the types, but contain
-- no data. That is, they are phantom types.
-----------------------------------------------------------------------------

module Data.Pattern.Base.TypeList where

infixr :++:

-- | Concatenation of lists. Instances:
--
-- > type instance Nil     :++: xs = xs
-- > type instance (h:*:t) :++: xs = h :*: (t :++: xs)
type family (:++:) (a :: [*]) (b :: [*]) :: [*]
type instance '[]      :++: xs = xs
type instance (h ': t) :++: xs = h ': (t :++: xs)