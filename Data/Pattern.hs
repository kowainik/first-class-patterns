-----------------------------------------------------------------------------
-- |
-- Module:      Data.Pattern
-- License:     BSD3
-- Maintainer:  Brent Yorgey <byorgey@cis.upenn.edu>
-- Stability:   experimental
-- Portability: non-portable (see .cabal)
--
-- The main module for first-class-patterns; to use the library it
-- should suffice to import this module.  For a quick start using the
-- library, see the examples below.
--
-- If you want to read further, start with "Data.Pattern.Base", which
-- defines the basic pattern type and some basic combinators.  Then
-- read "Data.Pattern.Common", which defines a number of convenient
-- combinators for constructing various sorts of patterns.
--
-- As an example, the following functions, @ex1@ and @ex2@, are
-- semantically equivalent:
--
-- @
--  ex1, ex2 :: Num a => Either a (a, a) -> a
--  ex1 a = 'match' a $
--            'left' ('cst' 4)         '->>' 0
--        '<|>' 'left' 'var'             '->>' id
--        '<|>' 'right' ('tup2' 'var' 'var') '->>' (+)
--  ex2 a = case a of
--            Left 4      -> 0
--            Left x      -> x
--            Right (x,y) -> x+y
-- @
--
-- Also, when optimisation is turned on, GHC will compile them to the
-- same code.
--
-- XXX add more examples here.
-----------------------------------------------------------------------------


module Data.Pattern (
  module Data.Pattern.Base,
  module Data.Pattern.Common
 ) where

import Data.Pattern.Base
import Data.Pattern.Common
