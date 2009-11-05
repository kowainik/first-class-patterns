-----------------------------------------------------------------------------
-- |
-- Module:      Data.Pattern.Base.TypeList
-- License:     BSD3
-- Maintainer:  Reiner Pope <reiner.pope@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Type-level lists. These lists only describe the types, but contain
-- no data. That is, they are phantom types.
-----------------------------------------------------------------------------

module Data.Pattern.Base.TypeList where

-- | The type of empty lists
data Nil

-- | @(:*:)@ corresponds to cons.
data h :*: t

infixr :*:
infixr :++:

-- | Concatenation of lists. Instances:
--
-- @ type instance Nil     :++: xs = xs
-- type instance (h:*:t) :++: xs = h :*: (t :++: xs)@
type family (:++:) a b
type instance Nil     :++: xs = xs
type instance (h:*:t) :++: xs = h :*: (t :++: xs)