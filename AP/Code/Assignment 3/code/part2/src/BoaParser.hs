-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
import Text.ParserCombinators.Parsec
import Data.Char
import Control.Applicative (liftA2)
-- add any other other imports you need

-- type ParseError = Err.ParseError -- you may replace this
-- definitions from the lecture
whitespace :: Parser ()
whitespace = do skipMany ( satisfy isSpace ); return ()

whitespace1 :: Parser ()
whitespace1 = do skipMany1 ( satisfy isSpace ); return ()

lexeme :: Parser a -> Parser a 
lexeme p = do a <- p ; whitespace ; return a

toString :: Parser Char -> Parser String
toString c = (:[]) <$> c

reservedKeywords = ["None", "True", "False", "for", "if", "in", "not"]

lu = ((satisfy isAlpha) <|> char '_')
ldu = (alphaNum <|> char '_')

ident :: Parser String
ident = lexeme $ do s <- lu
                    r <- many $ ldu
                    let id = (s:r)
                    if (id `elem` reservedKeywords) then fail (id ++ " is a reserved keyword.") else return id

numConst :: Parser Int
numConst = lexeme $ (read <$> (:[]) <$> (char '0')
                    <|>
                    ((char '-') >> (negate <$> numConst))
                    <|>
                    read <$> many1 digit)
stringConst :: Parser String
stringConst = lexeme $ between (toString $ char '\'') (toString $ char '\'') printEscaped


printableNoQBS :: Char -> Bool
printableNoQBS c = (isPrint c) && (not $ c `elem` ['\\', '\''])

-- parse everything as string, then concat. Because we need many 'something' where 'something' can be the empty string, thus we must use many over strings...
printEscaped :: Parser String
printEscaped = concat <$> (many $ ((toString (satisfy printableNoQBS)) --  printable chars without \ or '
                            <|>
                            (try ((char '\\') *> (string "\'")))  -- \'
                            <|>
                            (try ((char '\\') *> (string "\\")))  -- \\
                            <|>
                            (try ((char '\\' *> (char 'n') *> return "\n")))   -- \n
                            <|>
                            (((char '\\') *> (char '\n') >> return "")))) -- need string because we can't return empty char

-- testString = ['\'', 'f', 'o', '\\', '\\', 'o', '\\', '\n', 'b', '\\', 'n', 'a', '\\', '\'', 'r', '\'']
                  
expr :: Parser Exp
expr  = (Const <$> StringVal <$> stringConst)
        <|>
        (try $ liftA2 Call ident ((char '(') *> (expr `sepBy` (char ','))))
        <|>
        (string "not") *> (Not <$> expr)
        <|>
        (try $ List <$> between (toString $ char '[') (toString $ char ']') (expr `sepBy` (char ',')))
        <|>
        (between (toString $ char '[') (toString $ char ']') (listComp))



-- term :: Parser Exp
-- term t1 = factor
--           <|>
--           ((Oper Times t1) <$> (char '*' *> term))
--           <|> 
--           ((Oper Div t1) <$> (string "//" *> term))
--           <|> 
--           ((Oper Mod t1) <$> (char '%' *> term))

-- factor :: Parser Exp
-- factor = (Const <$> IntVal <$> numConst)
--          <|>
--          between (toString $ char '(') (toString $ char ')') expr
--          <|>
--          (try (Var <$> ident)) -- 2 cases that start with ident 
--          <|>
--          (string "None" *> return (Const NoneVal))
--          <|>
--          (string "False" *> return (Const FalseVal))
--          <|>
--          (string "True" *> return (Const TrueVal))

listComp :: Parser Exp
listComp = liftA2 Compr expr (forQual >>= (return $ (many $ (forQual <|> ifQual))))

ifQual :: Parser Qual
ifQual = (string "if") *> (QIf <$> expr)

forQual :: Parser Qual
forQual = (string "for") *> (liftA2 QFor ident expr)

stmt :: Parser Stmt
stmt =  (liftA2 SDef ident (char '=' >> expr))
        <|> 
        (SExp <$> expr)
program :: Parser Program
program = stmt `sepBy1` (char ';')


parseString :: String -> Either ParseError Program
parseString s = undefined -- parse ex "WarmupParsec.hs" s