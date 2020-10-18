module Parser where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus, empty, (<|>))
import Control.Lazy (class Lazy)
import Data.Array (head, tail, some)
import Data.Char.Unicode (isDigit)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))

type State = Array Char

newtype Parser a = Parser (State -> Maybe (Tuple State a))

runParser :: ∀ a. Parser a -> State -> Maybe (Tuple State a)
runParser (Parser p) s = p s

instance functorParser :: Functor Parser where
  map f pa = Parser \s -> do
    Tuple state a <- runParser pa s
    pure (Tuple state $ f a)

instance applyParser :: Apply Parser where
  apply pf pa = Parser \s -> do
    Tuple state f <- runParser pf s
    Tuple state' a <- runParser pa state
    pure (Tuple state' $ f a)

instance applicativeParser :: Applicative Parser where
  pure a = Parser \s -> Just (Tuple s a)

instance bindParser :: Bind Parser where
  bind pa f = Parser \s -> do
    Tuple s' a <- runParser pa s
    runParser (f a) s'

instance monadParser :: Monad Parser

instance altParser :: Alt Parser where
  alt pa pb = Parser \s ->
    case runParser pa s of
      Just res -> Just res
      Nothing -> runParser pb s

instance plusParser :: Plus Parser where
  empty = Parser \_ -> Nothing

instance alternativeParser :: Alternative Parser

instance lazyParser :: Lazy (Parser a) where
  defer f = Parser \s -> runParser (f unit) s

charP :: (Char -> Boolean) -> Parser Char
charP p = Parser \arr -> do
  x  <- head arr -- parsed
  xs <- tail arr -- new state
  if p x
  then pure $ Tuple xs x
  else Nothing

char :: Char -> Parser Char
char c = charP \c' -> c == c'

digit :: Parser Char
digit = charP isDigit

posNumber' :: Parser Int
posNumber' = do
  x <- map (fromString <<< fromCharArray) $ some digit
  case x of
    Just v -> pure v
    Nothing -> empty

posNumber :: Parser Int
posNumber =
  (char '+' *> posNumber')
  <|> posNumber'

negNumber :: Parser Int
negNumber = do
  _ <- char '-'
  x <- posNumber
  pure $ -x

-- TODO: make it work for floats
number :: Parser Int
number = posNumber <|> negNumber

parserToBool :: ∀ a. Parser a -> Parser Boolean
parserToBool p = Parser \s -> case runParser p s of
  Just (Tuple s' _)  -> Just $ Tuple s' true
  Nothing            -> Just $ Tuple s false

expr :: Parser Int
expr
   =  exprOperation '+' explSign (\x y -> x + y)
  <|> exprOperation '-' explSign (\x y -> x - y)
  <|> term
  where
    explSign = (char '+' <|> char '-') *> posNumber'

exprOperation :: ∀ a. Char -> Parser a -> (Int -> Int -> Int) -> Parser Int
exprOperation c p f = do
  x <- term
  _ <- char c
  isNext <- parserToBool p
  if isNext
  then empty
  else do
    y <- expr
    pure $ f x y

term :: Parser Int
term = do
  x <- factor
  _ <- char '*'
  isNext <- forbiden
  if isNext
  then empty
  else do
    y <- term
    pure $ x * y
  <|> factor
  where
    forbiden = parserToBool $ (char '+' <|> char '-') *> posNumber'

factor :: Parser Int
factor = do
  _ <- char '('
  x <- expr
  _ <- char ')'
  pure x
  <|> number
