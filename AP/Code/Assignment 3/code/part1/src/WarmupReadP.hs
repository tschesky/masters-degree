module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .

-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
-- Exp   ::= Term Exp' | "-" Term
-- Exp'  ::= e | Op Term Exp'
-- Op    ::= "+" | "-"
-- Term  ::= num | "(" Exp ")"

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
  -- may use instead of +++ for easier portability to Parsec

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

type ParseError = String  -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

parseString :: String -> Either ParseError Exp
parseString s = 
    case reverse $ readP_to_S expr s of
        [] -> Left "Arghhhhhhh"
        ((exp, _):_) -> Right exp

-- char - parser of Chars from ReadP
expr :: Parser Exp
expr = (term >>= expr')
    <|> (char '-' >> Negate <$> term)

expr' :: Exp-> Parser Exp
expr' e1 = (char '+' >> Add e1 <$> term >>= expr')
    <|> (char '-' >> Add e1 <$> (Negate <$> term) >>= expr')
    <|> (return e1)

-- between from ReadP
term :: Parser Exp
term = (Num <$> num)
    <|> (between (char '(') (char ')') expr)

-- num :: Parser Int
-- num = read <$> (munch1 (\c -> c `elem` "0123456789))
-- choice parser

some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p

num :: Parser Int
num = read <$> some (satisfy (\c -> c `elem` "0123456789"))