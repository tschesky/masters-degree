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

listComp :: Parser Exp
listComp = liftA2 Compr expr (forQual >>= (return $ (many $ (forQual <|> ifQual))))

ifQual :: Parser Qual
ifQual = (keyword "if") *> (QIf <$> expr)

forQual :: Parser Qual
forQual = (keyword "for") *> (liftA2 QFor ident (keyword "in" *> expr))

parseString :: String -> Either ParseError Program
parseString s = parse program "BoaParser" s

-- Note about eof and semicollons shit
program :: Parser Program
program = stmt `sepBy1` (symbol ';')

stmt :: Parser Stmt
stmt =  (try (liftA2 SDef ident (symbol '=' >> expr)))
        <|> 
        (SExp <$> expr)

expr :: Parser Exp
expr  = (keyword "not") *> (Not <$> expr)
        <|>
        relExpr        

keyword :: String -> Parser String
keyword k = (lexeme $ (string k))

symbol :: Char -> Parser Char
symbol k = (lexeme $ (char k))

relExpr :: Parser Exp
relExpr = (addExpr >>= relExpr')

relExpr' :: Exp -> Parser Exp
relExpr' e1 = try (relOp >>= (\oper -> ((oper e1) <$> addExpr) >>= relExpr'))
          <|> return e1

addExpr :: Parser Exp
addExpr = multExpr
          <|> 
          (multExpr >>= addExpr')

addExpr' :: Exp -> Parser Exp
addExpr' e1 = (addOp >>= (\oper -> ((oper e1) <$> multExpr) >>= addExpr'))
          <|> return e1


multExpr :: Parser Exp
multExpr = (term >>= multExpr')

multExpr' :: Exp -> Parser Exp
multExpr' e1 = (multOp >>= (\oper -> ((oper e1) <$> term) >>= multExpr'))
           <|> return e1

relOp :: Parser (Exp -> Exp -> Exp)
relOp = lexeme $ (keyword "==" *> (return $ Oper Eq)
                  <|>
                  keyword "!=" *> (return $ (\a b -> Not (Oper Eq a b)))
                  <|>
                  keyword ">=" *> (return $ (\a b -> Not (Oper Less a b)))
                  <|>
                  keyword "<=" *> (return $ (\a b -> Not (Oper Greater a b)))
                  <|>
                  keyword ">" *>  (return $ Oper Greater)
                  <|>
                  keyword "<" *>  (return $ Oper Less)
                  <|>
                  keyword "in" *> (return $ Oper In)
                  <|>
                  keyword "not in" *> (return $ (\a b -> Not (Oper In a b))))

addOp :: Parser (Exp -> Exp -> Exp)
addOp = lexeme $ ( symbol '+' *> (return $ Oper Plus)
                   <|>
                   symbol '-' *> (return $ Oper Minus))

multOp :: Parser (Exp -> Exp -> Exp)
multOp = lexeme $ ( symbol '*' *> (return $ Oper Plus)
                    <|>
                    string "//" *> (return $ Oper Div)
                    <|>
                    symbol '%' *> (return $ Oper Mod))

term :: Parser Exp
term = (try $ List <$> (brackets exprz))
       <|>
       (brackets listComp)
       <|>
       (Const <$> StringVal <$> stringConst)
       <|>
       (Const <$> IntVal <$> numConst)
       <|>
       (parens expr)
       <|>
       (keyword "None" *> return (Const NoneVal))
       <|>
       (keyword "False" *> return (Const FalseVal))
       <|>
       (keyword "True" *> return (Const TrueVal))
       <|>
       (ident >>= identFun)

identFun :: String -> Parser Exp
identFun e1 = (Call e1 <$> (parens exprz))
              <|>
              (return $ Var e1)

exprz :: Parser [Exp]
exprz = expr `sepBy` (symbol ',')

parens = between (symbol '(') (symbol ')')
brackets = between (toString $ symbol '[') (toString $ symbol ']')