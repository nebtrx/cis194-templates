{-# LANGUAGE InstanceSigs #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 10
--
----------------------------------------------------------------------

module AParser where

-- base
import Control.Applicative
import Data.Char


newtype Parser a =
  Parser { runParser :: String -> Maybe (a, String) }


-- |
--
-- >>> runParser (satisfy isUpper) "ABC"
-- Just ('A',"BC")
-- >>> runParser (satisfy isUpper) "abc"
-- Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f []          = Nothing
    f (x:xs)
      | p x       = Just (x, xs)
      | otherwise = Nothing


-- |
--
-- >>> runParser (char 'x') "xyz"
-- Just ('x',"yz")

char :: Char -> Parser Char
char c = satisfy (== c)


posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs


----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ fmap (first f) . runParser p

first :: (a -> b) -> (a,c) -> (b,c)
first f (x, y) = (f x, y)

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \s -> Just (a, s)
  
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  -- (<*>) (Parser f) (Parser a) = Parser $
  --   \s -> case f s of
  --     Nothing -> Nothing
  --     Just (f1, rest) -> fmap (first f1) (a rest)

  -- fmap :: (a -> b) -> f a -> f b
  -- transform :: (String -> Maybe (a, String)) -> Maybe(a -> b, String) -> Maybe(b, String)
  -- x :: String -> Maybe (a , String)
  -- f :: String -> Maybe (a -> b, String)
  
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) (Parser f) (Parser x) = Parser $ fmap (transform x)  f
    where
      transform :: (String -> Maybe (a, String)) -> Maybe(a -> b, String) -> Maybe(b, String)
      transform _ Nothing             = Nothing
      transform toMaybe (Just (g, s)) = fmap (\(y, str ) -> (g y, str)) (toMaybe s)
        


----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> runParser abParser "abcdef"
-- Just (('a','b'),"cdef")
-- >>> runParser abParser "aebcdf"
-- Nothing

abParser :: Parser (Char, Char)
abParser =  ( , )<$> char 'a' <*> char 'b'


-- |
--
-- >>> runParser abParser_ "abcdef"
-- Just ((),"cdef")
-- >>> runParser abParser_ "aebcdf"
-- Nothing

abParser_ :: Parser ()
-- (<$) :: Functor f => a -> f b -> f a
abParser_ = () <$ abParser


-- |
--
-- >>> runParser intPair "12 34"
-- Just ([12,34],"")

intPair :: Parser [Integer]
-- (<*) :: Applicative f => f a -> f b -> f a
-- runParser (posInt <* char ' ') "5 yy"
intPair =  couple <$> posInt <* char ' ' <*> posInt
  where
    couple x y = [x, y]


----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $  const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) p1 p2 = Parser (\s -> runParser p1 s <|> runParser p2 s )


----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

-- |
--
-- >>> runParser intOrUppercase "342abcd"
-- Just ((),"abcd")
-- >>> runParser intOrUppercase "XYZ"
-- Just ((),"YZ")
-- >>> runParser intOrUppercase "foo"
-- Nothing

intOrUppercase :: Parser ()
intOrUppercase = () <$ posInt <|> () <$ satisfy isUpper
