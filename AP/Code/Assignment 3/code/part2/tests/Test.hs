-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC
import Data.Char
import Control.Applicative (liftA2)

main :: IO ()
main = defaultMain $ localOption (mkTimeout 10000000) tests

tests :: TestTree
tests = testGroup "All tests" [minimalTests, propertyTst]

propertyTst :: TestTree
propertyTst = testGroup "Property tests" [prop_encode_decode]

prop_encode_decode = testGroup "Test encode-decode"
                [ QC.testProperty "Test encode-decode" $ (QC.withMaxSuccess 200
                    (\s -> let string = printStatement s in
                                 case parseString string of
                                  (Right (stmt:[])) ->  stmt QC.=== s
                                  a -> (Right []) QC.=== a
                    )),
                  QC.testProperty "Test encode-decode on whole program" $ (QC.withMaxSuccess 25
                    (\p -> let string = printProgram p in
                                case string of 
                                  "[]" -> 1 QC.=== 1
                                  "" -> 1 QC.=== 1
                                  s -> case parseString s of
                                        (Right stmts) ->  stmts QC.=== p
                                        a -> (Right []) QC.=== a
                    ))
                ]

minimalTests :: TestTree
minimalTests = testGroup "Minimal tests" [
  testCase "simple success" $
    parseString "2 + two" @?=
      Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
  testCase "simple failure" $
    -- avoid "expecting" very specific parse-error messages
    case parseString "wow!" of
      Left _ -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p]

-- identifier
newtype Identifier = Ident String deriving (Eq, Show)
instance QC.Arbitrary Identifier where
  arbitrary = Ident <$> (take 25) <$> (liftA2 (:) (QC.arbitrary `QC.suchThat` lu) (QC.listOf (QC.arbitrary `QC.suchThat` ldu))) `QC.suchThat` notKeyword

lu c = isAlpha c || (c == '_')
ldu c = (isAlpha c || (c == '_'))
printableNoQBS c = (isPrint c) && (not $ c `elem` ['\\', '\''])
notKeyword s = not $ s `elem` reservedKeywords
reservedKeywords = ["None", "True", "False", "for", "if", "in", "not"]

newtype StringConst = SC String deriving (Eq, Show)
instance QC.Arbitrary StringConst where
  arbitrary = SC <$> concat <$> QC.listOf validTokens

validTokens = liftA2 (:) ((QC.arbitrary `QC.suchThat` printableNoQBS)) (QC.elements (["\\\\","\\\'","\\n","\\\n"]))

-- value
instance QC.Arbitrary Value where
  arbitrary = val

-- we don't need listvalues since everything is parsed as List [exp] anyways
val =  QC.oneof $ [ return NoneVal,
                return TrueVal,
                return FalseVal,
                liftM IntVal QC.arbitrary,
                do (SC s) <- QC.arbitrary 
                   return $ StringVal (take 25 s)
            ]

-- expressions
instance QC.Arbitrary Exp where
  arbitrary = sizedExp

sizedExp = QC.sized expN

expN 0 = QC.oneof $ [ liftM Const QC.arbitrary,
                      (do (Ident name) <- QC.arbitrary
                          return $ Var name),
                      liftM Not QC.arbitrary,
                      return (List []),
                      (do o <- QC.arbitrary
                          v1 <- QC.arbitrary
                          v2 <- QC.arbitrary
                          return $ Oper o (Const v1) (Const v2)),
                      (do (Ident name) <- QC.arbitrary
                          v <- QC.arbitrary
                          return $ Call name [(Const v)]),
                      (do v1 <- QC.arbitrary
                          v2 <- QC.arbitrary
                          (Ident name) <- QC.arbitrary
                          return $ Compr (Const v1) [(QFor name (Const v2))])]

expN n = QC.oneof $ [ liftM Const QC.arbitrary,
                      (do (Ident name) <- QC.arbitrary
                          return $ Var name),
                      liftM Not QC.arbitrary,
                      return (List []),
                      (do o <- QC.arbitrary
                          e1 <- subExp
                          e2 <- subExp
                          return $ Oper o e1 e2),
                      (do (Ident name) <- QC.arbitrary
                          rest <- QC.arbitrary
                          return $ Call name [rest]),
                      (do e1 <- subExp
                          e2 <- subExp
                          (Ident name) <- QC.arbitrary
                          (Quals quals) <- subQual
                          return $ Compr e1 ((QFor name e2):quals))]
                      where subExp = expN (n `div` 4)
                            subQual = quals (n `div` 4)
-- operators
instance QC.Arbitrary Op where
  arbitrary = ops

ops = QC.elements $ [Plus, Minus, Times, Div, Mod, Eq, Less, Greater, In]


newtype Quals = Quals [Qual] deriving (Eq, Show)
instance QC.Arbitrary Quals where
  arbitrary = QC.sized quals 

quals 0 = QC.oneof $ [(do (Ident name) <- QC.arbitrary
                          v <- QC.arbitrary
                          return $ Quals $ ((QFor name (Const v):[]))),
                      (do v <- QC.arbitrary
                          return $ Quals $ ((QIf (Const v)):[]))]

quals n = QC.oneof $ [(do (Ident name) <- QC.arbitrary
                          e <- subExp
                          (Quals qs) <- subQuals
                          return $ Quals $ ((QFor name e):qs)),
                      (do e <- subExp
                          (Quals qs) <- subQuals
                          return $ Quals $ ((QIf e):qs)) ]
                      where subQuals = quals (n `div` 2)
                            subExp = expN (n `div` 4)
-- Statements
instance QC.Arbitrary Stmt where
  arbitrary = statements

statements =  QC.oneof $ [(do (Ident name) <- QC.arbitrary
                              exp <- QC.arbitrary
                              return $ SDef name exp),
                          (do exp <- QC.arbitrary
                              return $ SExp exp)]

------ Printing stuff -------
printVal :: Value -> String
printVal NoneVal = "None "
printVal TrueVal = "True "
printVal FalseVal = "False "
printVal (IntVal x) = show x
printVal (StringVal x) = "'" ++ (escapeStringVal x) ++ "'"
printVal (ListVal x) = "[" ++ (printValueL x True) ++ "]"


escapeStringVal :: String -> String
escapeStringVal "" = ""
escapeStringVal (('\''):rest) = "\\'" ++ (escapeStringVal rest)
escapeStringVal (('\\'):rest) = "\\\\" ++ (escapeStringVal rest)
escapeStringVal (('\n'):rest) = "\\n" ++ (escapeStringVal rest)
escapeStringVal (a:rest) = a : (escapeStringVal rest)

printx :: Eq a => (a -> String) -> String -> [a] -> Bool -> String
printx _ _ [] _ = ""
printx f sep (x:xs) il 
  | xs == [] = (f x)
  | otherwise = (if il then (f x) ++ sep else (f x) ++ " ") ++ (printx f sep xs il)

printValueL = printx printVal ", "
printL = printx printExpression ", "
printQualL = printx printQualifier " "
printExpL = printx printStatement ";"

printOperator :: Op -> String
printOperator Plus = "+"
printOperator Minus = "-"
printOperator Times = "*"
printOperator Div = "//"
printOperator Mod = "%"
printOperator Eq = "=="
printOperator Less = "<"
printOperator Greater = ">"
printOperator In = "in "

parens :: Exp -> String
parens e = "(" ++ (printExpression e)  ++ ")"

parens' :: String -> String
parens' e = "(" ++ e  ++ ")"

brackets :: String -> String
brackets e = "[" ++ e ++ "]"

printExpression :: Exp -> String
printExpression (Const val) = printVal val
printExpression (Var name) = name
printExpression (Oper op e1 e2) = (parens e1) ++ (printOperator op) ++ (parens e2)
printExpression (Not e1) = "not " ++ (parens e1)
printExpression (Call name args) = name ++ (parens' (printL args True))
printExpression (List elems) = (brackets (printL elems True))
printExpression (Compr exp quals) = "[" ++ (parens exp) ++ (printQualL quals True) ++ "]"

printQualifier :: Qual -> String
printQualifier (QFor name exp) = "for " ++ name ++ " in " ++(parens exp)
printQualifier (QIf exp) = "if " ++ (parens exp)

printStatement :: Stmt -> String
printStatement (SDef name exp) = name ++ "=" ++ (printExpression exp)
printStatement (SExp exp) = (printExpression exp)

printProgram :: Program -> String
printProgram p = printExpL p True