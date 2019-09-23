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
whitespace = skipMany ( satisfy isSpace )

whitespace1 :: Parser ()
whitespace1 = skipMany1 ( satisfy isSpace )

lexeme :: Parser a -> Parser a 
lexeme p = p <* whitespace

lexeme1 :: Parser a -> Parser a 
lexeme1 p = p <* whitespace1

toString :: Parser Char -> Parser String
toString c = (:[]) <$> c

reservedKeywords = ["None", "True", "False", "for", "if", "in", "not"]

lu = ((satisfy isAlpha) <|> char '_')
ldu = (alphaNum <|> char '_')

ident :: Parser String
ident = lexeme $ ((liftA2 (:) lu (many ldu)) >>= (\id -> if (id `elem` reservedKeywords) then fail (id ++ " is a reserved keyword.") else return id))
                    

numConst :: Parser Int
numConst = lexeme $ (read <$> (:[]) <$> (char '0') -- abuse no backtracking! 3rd case will not be used if leading 0 present
                    <|>
                    ((char '-') >> (negate <$> numConst))
                    <|>
                    read <$> many1 digit)

stringConst :: Parser String
stringConst = lexeme $ (char '\'') *> (printEscaped) <* (char '\'') 


printableNoQBS :: Char -> Bool
printableNoQBS c = (isPrint c) && (not $ c `elem` ['\\', '\''])

-- parse everything as string, then concat. Because we need many 'something' where 'something' can be the empty string, thus we must use many over strings...
printEscaped :: Parser String
printEscaped = concat <$> (many $ ((toString (satisfy printableNoQBS)) --  printable chars without \ or '
                            <|>
                            (try ((char '\\') *> (string "'")))  -- \'
                            <|>
                            (try ((char '\\') *> (string "\\")))  -- \\
                            <|>
                            (try ((char '\\' *> (char 'n') *> return "\n")))   -- \n
                            <|>
                            (((char '\\') *> (char '\n') *> return "")))) -- need string because we can't return empty char

listComp :: Parser Exp
listComp = liftA2 Compr expr (liftA2 (:) (forQual) (many $ (forQual <|> ifQual)))

ifQual :: Parser Qual
ifQual = (singletonKeyword "if") *> (QIf <$> expr)

forQual :: Parser Qual
forQual = (singletonKeyword "for") *> liftA2 QFor ident (singletonKeyword "in" *> expr)

parseString :: String -> Either ParseError Program
parseString s = (parse commentPreprocessor "Preprocessor" s) >>= (parse program "BoaParser" ) -- (\p -> Right $ [SExp $ Const $ StringVal p])

validEscapes = try (string "\\\\") <|> try (string "\\n") <|> try (string "\\\n") <|> try (string "\\'")

commentPreprocessor :: Parser String
commentPreprocessor = (concat <$> (many $ (
                                        (++ "'") <$> ("'"++) <$> ((char '\'') *> (concat <$> (many $ (toString $ satisfy $ printableNoQBS) <|> validEscapes)) <* (char '\'') ) 
                                        <|>
                                        (((char '#') *> skipMany (noneOf "\n") *> (skipMany1 newline <|> eof) *> return "\n"))
                                        <|> 
                                        ((:[]) <$> anyChar)))) <* eof
    
-- Note about eof and semicollons shit
program :: Parser Program
program = whitespace >> (stmt `sepBy1` (symbol ';')) <* eof

stmt :: Parser Stmt
stmt =  (try (liftA2 SDef ident (symbol '=' >> expr)))
        <|> 
        (try (SExp <$> expr))

expr :: Parser Exp
expr  = (try (singletonKeyword "not") *> (Not <$> expr))
        <|>
        relExpr        

keyword :: String -> Parser String
keyword k = (lexeme $ (string k))

keyword1 :: String -> Parser String
keyword1 k = (lexeme1 $ (string k))

singletonKeyword :: String -> Parser String
singletonKeyword s = (string s <* (notFollowedBy alphaNum) <* whitespace)

symbol :: Char -> Parser Char
symbol k = (lexeme $ (char k))

relExpr :: Parser Exp
relExpr = (addExpr >>= relExpr')

relExpr' :: Exp -> Parser Exp
relExpr' e1 = try (relOp >>= (\oper -> ((oper e1) <$> addExpr) >>= addExpr'))
          <|> return e1

addExpr :: Parser Exp
addExpr = (multExpr >>= addExpr')
      <|> multExpr

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
                  try( keyword ">=" *> (return $ (\a b -> Not (Oper Less a b))))
                  <|>
                  try( keyword "<=" *> (return $ (\a b -> Not (Oper Greater a b))))
                  <|>
                  keyword ">" *>  (return $ Oper Greater)
                  <|>
                  keyword "<" *>  (return $ Oper Less)
                  <|>
                  (string "in" <* notFollowedBy alphaNum <* whitespace) *> (return $ Oper In)
                  <|>
                  keyword1 "not" *> keyword "in" *> (return $ (\a b -> Not (Oper In a b))))

addOp :: Parser (Exp -> Exp -> Exp)
addOp = lexeme $ ( symbol '+' *> (return $ Oper Plus)
                   <|>
                   symbol '-' *> (return $ Oper Minus))

multOp :: Parser (Exp -> Exp -> Exp)
multOp = lexeme $ ( symbol '*' *> (return $ Oper Times)
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
       (try (singletonKeyword "None" *> return (Const NoneVal)))
       <|>
       (try (singletonKeyword "False" *> return (Const FalseVal)))
       <|>
       (try (singletonKeyword "True" *> return (Const TrueVal)))
       <|>
       (ident >>= identFun)

identFun :: String -> Parser Exp
identFun e1 = (Call e1 <$> (parens exprz))
              <|>
              (return $ Var e1)

exprz :: Parser [Exp]
exprz = expr `sepBy` (symbol ',')

parens = between (symbol '(') (symbol ')')
brackets = between (symbol '[') (symbol ']')