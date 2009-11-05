-----------------------------------------------------------------------------
-- |
-- Module:      Data.Pattern
-- License:     BSD3
-- Maintainer:  Reiner Pope <reiner.pope@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- The main import module for first-class-patterns. 
--
-- As an example, the following functions, @ex1@ and @ex2@ are
-- semantically equivalent: 
-- 
-- @  ex1, ex2 :: Num a => Either a (a, a) -> a
--  ex1 a = 'match' a $ 
--            'left' ('cst' 4)         '->>' 0
--        '<|>' 'left' 'var'             '->>' id
--        '<|>' 'right' ('tup2' 'var' 'var') '->>' (+)
--  ex2 a = case a of
--            Left 4      -> 0
--            Left x      -> x
--            Right (x,y) -> x+y@
-- 
-- Also, when optimisation is turned on, GHC will compile them to the
-- same code.
--
-----------------------------------------------------------------------------


module Data.Pattern (
  module Data.Pattern.Base,
  module Data.Pattern.Common,
  (<|>),
 ) where

import Data.Pattern.Base
import Data.Pattern.Common

import Control.Applicative((<|>))
