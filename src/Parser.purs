module Parser where

import Prelude

import Control.Alternative (empty, (<|>))
import Control.Monad.State (StateT(..), runStateT)
import Data.Array (head, tail, some)
import Data.Char.Unicode (isDigit)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))

type State = Array Char

type Parser a = StateT State Maybe a

runParser :: ∀ a. Parser a -> State -> Maybe (Tuple a State)
runParser = runStateT

charP :: (Char -> Boolean) -> Parser Char
charP p = StateT \arr -> do
  x  <- head arr -- parsed
  xs <- tail arr -- new state
  if p x
  then pure $ Tuple x xs
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
parserToBool p = StateT \s -> case runParser p s of
  Just (Tuple _ s')  -> Just $ Tuple true s'
  Nothing            -> Just $ Tuple false s

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
