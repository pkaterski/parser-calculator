module Parser where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

type State = String

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