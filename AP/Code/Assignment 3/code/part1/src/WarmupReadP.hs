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
import Data.Char
  -- may use instead of +++ for easier portability to Parsec

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

type ParseError = String  -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)
whitespace :: Parser ()
whitespace = (munch isSpace) *> return ()

lexeme :: Parser a -> Parser a 
lexeme p = p <* whitespace

symbol :: Char -> Parser ()
symbol s = (char s) *> whitespace *> return ()

parseString :: String -> Either ParseError Exp
parseString s = 
    case reverse $ readP_to_S simpleArith s of
        [] -> Left "Parse error."
        ((exp, _):_) -> Right exp 


simpleArith :: Parser Exp
simpleArith =  whitespace *> expr <* whitespace <* (eof)


-- char - parser of Chars from ReadP
expr :: Parser Exp
expr = (term >>= expr')
        <|>  ((symbol '-' >> Negate <$> term) >>= expr')
        <|> (symbol '-' >> Negate <$> term)

expr' :: Exp -> Parser Exp
expr' e1 = (symbol '+' >> (Add e1 <$> term) >>= expr')
    <|> (symbol '-' >> Add e1 <$> (Negate <$> term) >>= expr')
    <|> (return e1)

-- between from ReadP
term :: Parser Exp
term = (Num <$> num)
    <|> (between (symbol '(') (symbol ')') expr)
-- num :: Parser Int
-- num = read <$> (munch1 (\c -> c `elem` "0123456789))
-- choice parser

some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p

num :: Parser Int
num = lexeme $ read <$> some (satisfy (\c -> c `elem` "0123456789"))