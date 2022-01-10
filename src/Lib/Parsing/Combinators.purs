module Lib.Parsing.Combinators
  ( Parser(..)
  , ParserError(..)
  , anyChar
  , anySpace
  , bigint
  , char
  , digit
  , fail
  , finiteString
  , fromChars
  , isChar
  , isDigit
  , isSpace
  , liftParser
  , literal
  , runParser
  , someSpace
  , sringdigits
  , tail
  , takeuntil
  , ternary
  , toChars
  , ignorecase
  ) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Lazy (class Lazy)
import Control.Plus (class Plus)
import Data.Array (fromFoldable, index, toUnfoldable)
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Data.List (List(..), many, (:), some, foldl)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), drop, length, split, toLower)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))

foreign import isDigitImpl :: Char -> Boolean

foreign import isSpaceImpl :: Char -> Boolean

data Parser a
  = Parser (String -> Either ParserError (Tuple a String))

newtype ParserError
  = ParserError String

derive newtype instance showParserError :: Show ParserError

derive newtype instance eqParserError :: Eq ParserError

runParser ::
  forall a.
  Parser a ->
  String ->
  Either ParserError (Tuple a String)
runParser (Parser p) s = p s

instance functorParser :: Functor Parser where
  map f p =
    Parser \s -> case runParser p s of
      Right (Tuple x xs) -> Right $ Tuple (f x) xs
      Left error -> Left error

instance applyParser :: (Functor Parser) => Apply Parser where
  apply fg f =
    Parser \s -> case runParser fg s of
      Right (Tuple x xs) -> case runParser f xs of
        Right (Tuple v vs) -> Right $ Tuple (x v) vs
        Left error -> Left error
      Left error -> Left error

instance applicativeParser :: (Apply Parser) => Applicative Parser where
  pure x = Parser \s -> Right $ Tuple x s

instance bindParser :: (Apply Parser) => Bind Parser where
  --bind :: forall a b. m a -> (a -> m b) -> m b
  bind m g =
    Parser \s -> case runParser m s of
      Right (Tuple x xs) -> runParser (g x) xs
      Left error -> Left error

instance monadParser :: (Bind Parser) => Monad Parser

instance altParser :: (Functor Parser) => Alt Parser where
  alt a b =
    Parser \s -> case runParser a s of
      Right r -> Right r
      Left _ -> runParser b s

instance plusParser :: (Alt Parser) => Plus Parser where
  empty = fail "could not parse"

instance alternativeParser :: (Alt Parser, Plus Parser) => Alternative Parser

instance lazyParser :: Lazy (Parser (List a)) where
  defer f = Parser \s -> runParser (f unit) s

liftParser :: forall a. a -> Parser a
liftParser a = Parser \s -> Right $ Tuple a s

isSpace :: Char -> Boolean
isSpace = isSpaceImpl

isDigit :: Char -> Boolean
isDigit = isDigitImpl

isChar :: Char -> Char -> Boolean
isChar a b = a == b

toChars :: String -> List Char
toChars = toCharArray >>> toUnfoldable

fromChars :: List Char -> String
fromChars = fromFoldable >>> fromCharArray

fail :: forall a. String -> Parser a
fail reason = Parser \_ -> Left $ ParserError reason

anyChar :: Parser Char
anyChar = Parser (toChars >>> f)
  where
  f (x : xs) = Right (Tuple x (foldl (\acc s -> acc <> fromCharArray [ s ]) "" xs))

  f Nil = Left $ ParserError ("Cannot parse empty charlist")

ternary :: (Char -> Boolean) -> Parser Char
ternary pred = do
  c <- anyChar
  if pred c then
    pure c
  else
    fail ("cannot match " <> (fromChars (Cons c Nil)))

char :: Char -> Parser Char
char x = ternary ((==) x)

digit :: Parser Char
digit = ternary isDigitImpl

literal :: String -> Parser String
literal s =
  fromChars
    <$> case toChars s of
        Nil -> pure Nil
        (x : xs) -> do
          _ <- char x
          _ <- literal $ fromChars xs
          pure $ Cons x xs

-- literal :: String -> Parser String
-- literal s = do
--   _ <- match s
--   pure $ s
finiteString :: Parser String
finiteString = do
  s <- many $ ternary (isSpace >>> not)
  pure $ fromChars s

tail :: Parser String
tail = Parser \s -> Right $ Tuple s ""

sringdigits :: Parser String
sringdigits = do
  s <- many $ ternary isDigit
  pure $ fromChars s

-- takeuntil :: Char -> Parser String
-- takeuntil match' = do
--   s <- many $ ternary (isChar match' >>> not)
--   pure $ fromChars (s)
takeuntil :: String -> Parser String
takeuntil matchpattern =
  Parser \s -> do
    let
      parts = split (Pattern matchpattern) s

      head = index parts 0
    case head of
      Nothing -> Right $ Tuple s ""
      Just heads' -> do
        let
          rest = drop (length heads') s
        Right $ Tuple heads' rest

--case splitAt (length )
bigint :: Parser (Maybe BigInt.BigInt)
bigint = do
  s <- sringdigits
  pure $ BigInt.fromString s

anySpace :: Parser String
anySpace = do
  spaces' <- many $ ternary isSpace
  pure $ fromChars spaces'

someSpace :: Parser String
someSpace = do
  spaces' <- some $ ternary isSpace
  pure $ fromChars spaces'

ignorecase :: String -> Parser String
ignorecase s = 
  (literal s) <|> (literal $ toLower s)
