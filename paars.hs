{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonadComprehensions #-}

import Control.Applicative (liftA)
import Control.Monad (MonadPlus)
import Data.Bifunctor (first)
import Data.Char
import GHC.Base

newtype Paars a = Paars (String -> [(a, String)])

instance Functor Paars where
  fmap :: (a -> b) -> Paars a -> Paars b
  fmap = liftA

instance Applicative Paars where
  pure :: a -> Paars a
  pure = result

  (<*>) :: Paars (a -> b) -> Paars a -> Paars b
  Paars pAB <*> Paars pA = Paars (concatMap (\(f, inp') -> map (first f) (pA inp')) . pAB)

-- todo: is this equivalent?
-- Paars (\inp -> [(ab a, inp'') | (a, inp') <- pA inp, (ab, inp'') <- pAB inp'])

instance Monad Paars where
  return :: a -> Paars a
  return = pure

  (>>=) :: Paars a -> (a -> Paars b) -> Paars b
  (>>=) = bind

instance Alternative Paars where
  empty = zero
  (<|>) = plus

instance MonadPlus Paars

run :: Paars a -> String -> [(a, String)]
run (Paars f) = f

-- always succeeds, returning val and unmodified input
result :: a -> Paars a
result val = Paars (\inp -> [(val, inp)])

-- always fails
zero :: Paars a
zero = Paars (const [])

-- returns the first character of input
item :: Paars Char
item = Paars item'
  where
    item' (c : cs) = [(c, cs)]
    item' _ = []

-- takes a parser of as and a function from a to a parser of bs
-- returns a parser of bs
-- the resultant parser applies func to all results of pA
bind :: Paars a -> (a -> Paars b) -> Paars b
Paars pA `bind` func = Paars (concatMap (\(v, inp') -> run (func v) inp') . pA)

-- takes two parsers of as and returns a parser of as
-- the resultant parser returns results from both parsers
plus :: Paars a -> Paars a -> Paars a
Paars p1 `plus` Paars p2 = Paars (\inp -> p1 inp ++ p2 inp)

-- parses a char iff it satisfies predicate
sat :: (Char -> Bool) -> Paars Char
sat predicate = [x | x <- item, predicate x]

-- some useful parsers
char :: Char -> Paars Char
char c = sat (c ==)

digit :: Paars Char
digit = sat isDigit

lower :: Paars Char
lower = sat isLower

upper :: Paars Char
upper = sat isUpper

letter :: Paars Char
letter = lower <|> upper

alphanum :: Paars Char
alphanum = letter <|> digit

word :: Paars String
word = [c : cs | c <- letter, cs <- word] <|> result ""

-- equivalent to either of:
-- neWord = do
--   c <- letter
--   cs <- word
--   return (c : cs)
-- neWord =
--   letter `bind` \c ->
--     word `bind` \cs ->
--       result (c : cs)

string :: String -> Paars String
string "" = result ""
string (c : cs) = [c : cs | _ <- char c, _ <- string cs]

byte :: Paars Int
byte = [val | x <- digit, let val = digitToInt x, val < 8]
