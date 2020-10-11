module Parser where

import Prelude

import Control.Alternative (class Alt, class Plus, class Alternative)
import Control.Lazy (class Lazy)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

type State = Array Char

newtype Parser a = Parser (State -> Maybe (Tuple State a))

runParser :: ∀ a. Parser a -> State -> Maybe (Tuple State a)
runParser (Parser p) s = p s

instance functorParser :: Functor Parser where
    map f pa = Parser \s -> do
        Tuple state a <- runParser pa s
        pure (Tuple state $ f a)

-- shit
-- instance applyParser :: Apply Parser where
--     apply pf pa = Parser \s ->
--         case runParser s pf of
--             Nothing-> Nothing
--             Just (Tuple state f) ->
--                 case runParser state pa of
--                     Nothing-> Nothing
--                     Just (Tuple state' a) -> Just (Tuple state' $ f a)

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
        runParser (f a) s

-- ??
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
