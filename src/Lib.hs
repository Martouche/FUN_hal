module Lib where

import           Control.Applicative
import           Data.Foldable
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Text.Read           (readMaybe)

newtype Parser a =
  Parser (String -> Maybe (String, a))

charParser :: Char -> Parser Char
charParser c = Parser p
  where
    p x
      | length x <= 0 = Nothing
      | head x == c = Just (tail x, c)
      | otherwise = Nothing

predParser :: (Char -> Bool) -> Parser Char
predParser pred = Parser p
  where
    p x
      | length x <= 0 = Nothing
      | pred $ head x = Just (tail x, head x)
      | otherwise = Nothing

instance Functor Parser where
  fmap f (Parser p) =
    Parser
      (\x ->
         case p x of
           Just (rest, res) -> Just (rest, f res)
           Nothing          -> Nothing)

instance Applicative Parser where
  pure x = Parser (\y -> Just (y, x))
  liftA2 f (Parser p) (Parser p') =
    Parser
      (\x ->
         case p x of
           Nothing -> Nothing
           Just (rest, res) ->
             case p' rest of
               Nothing            -> Nothing
               Just (rest', res') -> Just (rest', f res res'))

instance Alternative Parser where
  empty = Parser (\x -> Nothing)
  (<|>) (Parser p) (Parser p') =
    Parser
      (\x ->
         case p x of
           Just (rest, res) -> Just (rest, res)
           Nothing          -> p' x)

parse :: String -> Parser a -> Maybe (a, String)
parse str (Parser p) =
  case p str of
    Nothing          -> Nothing
    Just (rest, res) -> Just (res, rest)

choice :: (Alternative f) => [f a] -> f a
choice = foldl1 (<|>)

parseSeparators :: Parser ()
parseSeparators = void $ many $ choice [charParser ' ', charParser '\t', charParser '\n']

parseNonSeparators :: Parser String
parseNonSeparators = some $ predParser (\x -> notElem x [' ', '\t', '\n', '(', ')'])

data Expr
  = Value String
  | SubExpr [Expr]
  deriving (Show)

instance Eq Expr where
  (==) (Value x) (Value y)              = x == y
  (==) (SubExpr exprs) (SubExpr exprs') = exprs == exprs'

toString :: Expr -> String
toString (Value v) = v
toString (SubExpr [Value "'", e]) = "'" ++ toString e
toString (SubExpr xs) = "(" ++ intercalate " " (map toString xs) ++ ")"

parseValue :: Parser Expr
parseValue = fmap Value parseNonSeparators

parseQuote :: Parser Expr
parseQuote = fmap (\x -> SubExpr [Value "'", x]) (charParser '\'' *> parseSeparators *> parseExpr)

parseFunc :: Parser Expr
parseFunc = charParser '(' *> (fmap SubExpr (many (parseSeparators *> parseExpr))) <* parseSeparators <* charParser ')'

parseExpr :: Parser Expr
parseExpr = parseSeparators *> choice [parseQuote, parseValue, parseFunc]

readIntExpr :: Expr -> Maybe Integer
readIntExpr x =
  case hal x of
    Nothing        -> Nothing
    Just (Value v) -> readMaybe v
    Just _         -> Nothing

binaryNumberOp :: (Integer -> Integer -> Integer) -> [Expr] -> Maybe Integer
binaryNumberOp _ exprs
  | length exprs < 2 = Nothing
binaryNumberOp op exprs = foldl1 (\x y -> liftA2 op x y) (map readIntExpr exprs)

hal :: Expr -> Maybe Expr
hal (Value v) = Just (Value v)
hal (SubExpr [Value "'", e]) = Just $ e
hal (SubExpr [Value "quote", e]) = Just $ e
hal (SubExpr (Value "+":exprs)) = fmap (\x -> Value $ show x) (binaryNumberOp (+) exprs)
hal (SubExpr (Value "-":exprs)) = fmap (\x -> Value $ show x) (binaryNumberOp (-) exprs)
hal (SubExpr (Value "*":exprs)) = fmap (\x -> Value $ show x) (binaryNumberOp (*) exprs)
hal (SubExpr (Value "div":exprs)) = fmap (\x -> Value $ show x) (binaryNumberOp div exprs)
hal (SubExpr [Value "<", x, y]) =
  case liftA2 (<) (readIntExpr x) (readIntExpr y) of
    Just True  -> Just $ Value "#t"
    Just False -> Just $ Value "#f"
    Nothing    -> Nothing
hal (SubExpr [Value "eq?", x, y]) =
  case liftA2 (==) (hal x) (hal y) of
    Just True  -> Just $ Value "#t"
    Just False -> Just $ Value "#f"
    Nothing    -> Nothing
hal (SubExpr [Value "atom?", e]) =
  case hal e of
    Nothing           -> Nothing
    Just (SubExpr []) -> Just $ Value "#t"
    Just (Value _)    -> Just $ Value "#t"
    Just _            -> Just $ Value "#f"
hal (SubExpr [Value "cons", x, y]) =
  case hal x of
    Nothing -> Nothing
    Just (SubExpr _) -> Nothing
    Just (Value x') ->
      case hal y of
        Nothing -> Nothing
        Just (Value y') -> Just $ SubExpr [Value x', Value ".", Value y']
        Just (SubExpr exprs) -> Just $ SubExpr $ [Value x'] ++ exprs
hal _ = Nothing
