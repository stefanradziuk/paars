{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonadComprehensions #-}

import Control.Applicative (liftA)
import Control.Monad (MonadPlus)
import Data.Bifunctor (first)
import Data.Char
import GHC.Base (Alternative, empty, (<|>))

newtype Paars a = Paars (String -> [(a, String)])

instance Functor Paars where
  fmap :: (a -> b) -> Paars a -> Paars b
  fmap = liftA

instance Applicative Paars where
  -- always succeeds, returning val and unmodified input
  pure :: a -> Paars a
  pure val = Paars (\inp -> [(val, inp)])

  (<*>) :: Paars (a -> b) -> Paars a -> Paars b
  Paars pAB <*> Paars pA = Paars (concatMap (\(f, inp') -> map (first f) (pA inp')) . pAB)

-- todo: is this equivalent?
-- Paars (\inp -> [(ab a, inp'') | (a, inp') <- pA inp, (ab, inp'') <- pAB inp'])

instance Monad Paars where
  return :: a -> Paars a
  return = pure

  -- takes a parser of as and a function from a to a parser of bs
  -- returns a parser of bs
  -- the resultant parser applies func to all results of pA
  (>>=) :: Paars a -> (a -> Paars b) -> Paars b
  Paars pA >>= func = Paars (concatMap (\(v, inp') -> run (func v) inp') . pA)

instance Alternative Paars where
  -- always fails
  empty :: Paars a
  empty = Paars (const [])

  -- takes two parsers of as and returns a parser of as
  -- the resultant parser returns results from both parsers
  (<|>) :: Paars a -> Paars a -> Paars a
  Paars p1 <|> Paars p2 = Paars (\inp -> p1 inp ++ p2 inp)

instance MonadPlus Paars

run :: Paars a -> String -> [(a, String)]
run (Paars f) = f

-- returns the first character of input
item :: Paars Char
item = Paars item'
  where
    item' (c : cs) = [(c, cs)]
    item' _ = []

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

string :: String -> Paars String
string "" = pure ""
string (c : cs) = [c : cs | _ <- char c, _ <- string cs]

byte :: Paars Int
byte = [val | x <- digit, let val = digitToInt x, val < 8]

-- repetition parsers

-- todo: naming?
many :: Paars a -> Paars [a]
many pA = [x : xs | x <- pA, xs <- many pA] <|> pure []

many1 :: Paars a -> Paars [a]
many1 pA = [x : xs | x <- pA, xs <- many pA]

word :: Paars String
word = many letter

nat :: Paars Int
nat = [read xs | xs <- many1 digit]

int :: Paars Int
int = nat <|> [- n | _ <- char '-', n <- nat, n /= 0]

-- int can also be defined as:
--
-- int = [f n | f <- operator, n <- nat, n /= 0]
--   where
--     operator = [negate | _ <- char '-'] <|> pure id
--
-- on input "-123": operator = [(negate, "123"), (id, "-123")]
--                          n <- nat will fail on this ^
--                          so id is not used in this case
