-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC
import Text.ParserCombinators.Parsec

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "All tests" [minimalTests, propertyTst]

propertyTst :: TestTree
propertyTst = testGroup "Property tests" [prop_encode_decode]

-- Operate on operator In should always retun the same value as elem function from Haskell
prop_encode_decode = testGroup "Test encode-decode"
                [ QC.testProperty "Test encode-decode" $
                    (\s -> let string = printStatement s in
                                 let exp = parseString string in
                                   s == exp)
                ]

minimalTests :: TestTree
minimalTests = testGroup "Minimal tests" [
  testCase "simple success" $
    parseString "2 + two" @?=
      Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
  testCase "simple failure" $
    -- avoid "expecting" very specific parse-error messages
    case parseString "wow!" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p]

-- test types
newtype ListValue = LV Value deriving (Eq, Show)
instance QC.Arbitrary ListValue where
    arbitrary = QC.sized listVal

-- list
instance QC.Arbitrary Value where
    arbitrary = sizedVal

sizedVal = QC.sized valN

valN 0 =  QC.oneof $ [ return NoneVal,
                return TrueVal,
                return FalseVal,
                liftM IntVal QC.arbitrary,
                liftM StringVal QC.arbitrary,
                return (ListVal [])
            ]
valN n = QC.oneof [return NoneVal,
                    return TrueVal,
                    return FalseVal,
                    liftM IntVal QC.arbitrary,
                    liftM StringVal QC.arbitrary,
                    do res <- subVal
                       return (ListVal [res])
                   ]
                where subVal = (valN (n `div` 2))

listVal n = do res <- subVal
               return $ LV (ListVal [res])
            where subVal = (valN (n `div` 2))

-- expressions
instance QC.Arbitrary Exp where
  arbitrary = sizedExp

sizedExp = QC.sized expN

expN 0 = QC.oneof $ [ liftM Const QC.arbitrary,
                      liftM Var QC.arbitrary,
                      liftM Not QC.arbitrary,
                      return (List []),
                      (do o <- QC.arbitrary
                          e1 <- QC.arbitrary
                          e2 <- QC.arbitrary
                          return $ Oper o (Const e1) (Const e2)),
                      (do name <- QC.arbitrary
                          return $ Call name []),
                      (do e <- QC.arbitrary
                          return $ Compr (Const e) [])]

expN n = QC.oneof $ [ liftM Const QC.arbitrary,
                      liftM Var QC.arbitrary,
                      liftM Not QC.arbitrary,
                      return (List []),
                      (do o <- QC.arbitrary
                          e1 <- subExp
                          e2 <- subExp
                          return $ Oper o e1 e2),
                      (do name <- QC.arbitrary
                          rest <- subExp
                          return $ Call name [rest]),
                      (do e <- QC.arbitrary
                          quals <- subQual
                          return $ Compr (Const e) [quals])]
                  where subExp = (expN (n `div` 2))
                        subQual = (quals (n `div` 2))

-- operators
instance QC.Arbitrary Op where
  arbitrary = ops

ops = QC.elements $ [Plus, Minus, Times, Div, Mod, Eq, Less, Greater, In]

-- qualifiers
instance QC.Arbitrary Qual where
  arbitrary = sizedQuals

sizedQuals = QC.sized quals 

quals 0 = QC.oneof $ [(do name <- QC.arbitrary
                          e <- QC.arbitrary
                          return $ QFor name (Const e)),
                      (do e <- QC.arbitrary
                          return $ QIf (Const e))]

quals n = QC.oneof $ [(do name <- QC.arbitrary
                          e <- subExp
                          return $ QFor name e),
                      (do e <- subExp
                          return $ QIf e)]
                    where subExp = (expN (n `div` 2))

-- Statements
instance QC.Arbitrary Stmt where
  arbitrary = statements

statements =  QC.oneof $ [(do name <- QC.arbitrary
                              exp <- QC.arbitrary
                              return $ SDef name exp),
                          (do exp <- QC.arbitrary
                              return $ SExp exp)]


------ Printing stuff -------
printVal :: Value -> String
printVal NoneVal = "None"
printVal TrueVal = "True"
printVal FalseVal = "False"
printVal (IntVal x) = show x
printVal (StringVal x) = x
printVal (ListVal x) = "[" ++ (print' x True) ++ "]"

-- Bool indicates if value is in list
print' :: [Value] -> Bool -> String
print' [] _ = ""
print' (x:xs) il
  | xs == [] = (printVal x)
  | otherwise = (if il then (printVal x) ++ ", " else (printVal x) ++ " ") ++ (print' xs il)

-- Bool indicates if value is in list
print'' :: [Exp] -> Bool -> String
print'' [] _ = ""
print'' (x:xs) il
   | xs == [] = (printExpression x)
   | otherwise = (if il then (printExpression x) ++ ", " else (printExpression x) ++ " ") ++ (print'' xs il)

-- Bool indicates if value is in list
print''' :: [Qual] -> Bool -> String
print''' [] _ = ""
print''' (x:xs) il
    | xs == [] = (printQualifier x)
    | otherwise = (if il then (printQualifier x) ++ ", " else (printQualifier x) ++ " ") ++ (print''' xs il)

printOperator :: Op -> String
printOperator Plus = "+"
printOperator Minus = "-"
printOperator Times = "*"
printOperator Div = "//"
printOperator Mod = "%"
printOperator Eq = "=="
printOperator Less = "<"
printOperator Greater = ">"
printOperator In = "in"

parens :: Exp -> String
parens e = "(" ++ (printExpression e)  ++ ")"

parens' :: String -> String
parens' e = "(" ++ e  ++ ")"

brackets :: String -> String
brackets e = "[" ++ show e ++ "]"

printExpression :: Exp -> String
printExpression (Const val) = printVal val
printExpression (Var name) = name
printExpression (Oper op e1 e2) = (printExpression e1) ++ (printOperator op) ++ (printExpression e2)
printExpression (Not e1) = "not " ++ (parens e1)
printExpression (Call name args) = name ++ (parens' (print'' args True))
printExpression (List elems) = (brackets (print'' elems True))
printExpression (Compr exp quals) = "[" ++ (parens exp) ++ (print''' quals True) ++ "]"

printQualifier :: Qual -> String
printQualifier (QFor name exp) = "for " ++ name ++ (parens exp)
printQualifier (QIf exp) = "in " ++ (parens exp)

printStatement :: Stmt -> String
printStatement (SDef name exp) = name ++ (printExpression exp)
printStatement (SExp exp) = (printExpression exp)
