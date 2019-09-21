module WarmupParsec where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
-- E ::= T E' | "-" T
-- E'::= e | "+" T E' | "-" T E'
-- T ::= num | "(" E ")"

import Text.ParserCombinators.Parsec  -- exports a suitable type ParseError
import Data.Char
data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

ex :: Parser Exp 
ex = (term >>= ex')
     <|> 
     (char '-' >> Negate <$> term)

ex' :: Exp -> Parser Exp
-- (char '+' >> Add e1 <$> term >>= expr')
ex' t1 = ((char '+') >> (Add t1 <$> term) >>= ex')
          <|> 
          ((char '-') >> (Add t1 <$> Negate <$> term) >>= ex')
          <|> 
          return t1

term :: Parser Exp
term = Num <$> num
       <|>
       between (char '(') (char ')') ex 
      

num :: Parser Int
num = read <$> many1 (satisfy isDigit)

-- Optional: if not attempted, leave as undefined
parseString :: String -> Either ParseError Exp
parseString s = parse ex "WarmupParsec.hs" s
