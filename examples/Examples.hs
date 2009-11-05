module Examples where

import Control.Applicative

import Data.Pattern

--- Basic pattern matching syntax
ex1, ex2 :: Either Int (Int, Int) -> Int
ex1 a = match a $ 
          left (cst 4)         ->> 0
      <|> left var             ->> id
      <|> right (tup2 var var) ->> (+)

ex2 a = case a of
          Left 4      -> 0
          Left x      -> x
          Right (x,y) -> x+y

-- Defining your own pattern matchers.
data Foo = Foo1 Int Int Int Int Int | Foo2 Int

foo1 :: Pat5 Int Int Int Int Int Foo
foo1 = mk5 (\x -> case x of
                    Foo1 a b c d e -> Just (a,b,c,d,e)
                    _              -> Nothing)

foo2 :: Pat1 Int Foo
foo2 = mk1 (\x -> case x of
                    Foo2 a -> Just a
                    _      -> Nothing)

ex3 :: Foo -> Int
ex3 a = match a $
            foo1 var (cst 5) __ __ __ ->> id
        <|> foo1 __ var __ __ __      ->> id
        <|> foo2 var                  ->> id

-- using Functor/Applicative/Monad instances on Clause.

-- Functor:   apply a function to multiple cases, using (<$>).
ex4 :: Either (Int,Int) Int -> Int
ex4 a = match a $
           (1+) <$> (left (tup2 var (cst 4)) ->> id
                 <|> right var               ->> id)
       <|> left (tup2 __ var) ->> id

--ex4' :: Either (Int,Int) Int -> Int
--ex4' a = match a $ 
            

-- Applicative:  do 2 pattern matches on the same data, and combine them with function application. (<*>).
ex5 :: (Int,Int) -> Int
ex5 a = match a $ 
            (tup2 (cst 4) __ ->> (3*) <|> tup2 var __ ->> (*)) <*> (tup2 __ var ->> id)

-- ex5 is semantically the same as ex5'.
ex5' :: (Int,Int) -> Int
ex5' a = case a of
           (4,_) -> case a of
                      (_,y) -> 3 * y
           (x,_) -> case a of
                      (_,y) -> x * y

---- "anonymous" matching
ex6 :: Show a => Either a String -> IO ()
ex6 = elim $
            left  var ->> print
        <|> right var ->> putStrLn

-- same as Prelude.either
ex7 :: (a -> r) -> (b -> r) -> Either a b -> r
ex7 withLeft withRight = elim $
             left  var ->> withLeft
         <|> right var ->> withRight


-- "monadic" matching
ex8 :: IO ()
ex8 = mmatch getLine $
        cst "" ->> return ()
    <|> var       ->> putStrLn . ("You said " ++)