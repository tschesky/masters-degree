-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC
import Text.ParserCombinators.Parsec
import Data.Char
import Control.Applicative (liftA2)

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "All tests" [minimalTests, propertyTst]

propertyTst :: TestTree
propertyTst = testGroup "Property tests" [prop_encode_decode]

prop_encode_decode = testGroup "Test encode-decode"
                [ QC.testProperty "Test encode-decode" $
                    (\s -> let string = printStatement s in
                                 case parseString string of
                                  (Right (exp:_)) -> s QC.=== exp
                                  _ -> QC.property QC.Discard
                    )
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

-- identifier
newtype Identifier = Ident String deriving (Eq, Show)
instance QC.Arbitrary Identifier where
  arbitrary = Ident <$> (liftA2 (:) (QC.arbitrary `QC.suchThat` lu) (QC.listOf (QC.arbitrary `QC.suchThat` ldu)))

lu c = isAlpha c || (c == '_')
ldu c = (isAlpha c || (c == '_'))
printableNoQBS c = (isPrint c) && (not $ c `elem` ['\\', '\''])

newtype StringConst = SC String deriving (Eq, Show)
instance QC.Arbitrary StringConst where
  arbitrary = SC <$> concat <$> QC.listOf validTokens

validTokens = liftA2 (:) ((QC.arbitrary `QC.suchThat` printableNoQBS)) (QC.elements (["\\\\","\\\'","\\n","\\\n"]))

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
-- newtype TestExp = TExp Exp deriving (Show)
-- instance Eq TestExp where
--   (==) (TExp (List [])) (TExp (Const (ListVal []))) = True
--   (==) (TExp (Const (ListVal []))) (TExp (List []) = True
-- instance EQ ListVal where
--   (List []) == (Const $ ListVal []) = True
--   (Const $ ListVal []) == (List []) = True
  

sizedExp = QC.sized expN

expN 0 = QC.oneof $ [ liftM Const QC.arbitrary,
                      (do (Ident name) <- QC.arbitrary
                          return $ Var name),
                      liftM Not QC.arbitrary,
                      return (List []),
                      (do o <- QC.arbitrary
                          e1 <- QC.arbitrary
                          e2 <- QC.arbitrary
                          return $ Oper o (Const e1) (Const e2)),
                      (do (Ident name) <- QC.arbitrary
                          e <- QC.arbitrary
                          return $ Call name [e]),
                      (do e1 <- QC.arbitrary
                          e2 <- QC.arbitrary
                          (Ident name) <- QC.arbitrary
                          return $ Compr e1 [(QFor name e2)])]

expN n = QC.oneof $ [ liftM Const QC.arbitrary,
                      (do (Ident name) <- QC.arbitrary
                          return $ Var name),
                      liftM Not QC.arbitrary,
                      return (List []),
                      (do o <- QC.arbitrary
                          e1 <- QC.arbitrary
                          e2 <- QC.arbitrary
                          return $ Oper o e1 e2),
                      (do (Ident name) <- QC.arbitrary
                          rest <- QC.arbitrary
                          return $ Call name [rest]),
                      (do e1 <- QC.arbitrary
                          e2 <- QC.arbitrary
                          (Ident name) <- QC.arbitrary
                          quals <- QC.arbitrary
                          return $ Compr e1 ((QFor name e2):[quals]))]
-- operators
instance QC.Arbitrary Op where
  arbitrary = ops

ops = QC.elements $ [Plus, Minus, Times, Div, Mod, Eq, Less, Greater, In]

-- qualifiers
instance QC.Arbitrary Qual where
  arbitrary = sizedQuals

sizedQuals = QC.sized quals 

quals 0 = QC.oneof $ [(do (Ident name) <- QC.arbitrary
                          e <- QC.arbitrary
                          return $ QFor name (Const e)),
                      (do e <- QC.arbitrary
                          return $ QIf (Const e))]

quals n = QC.oneof $ [(do (Ident name) <- QC.arbitrary
                          e <- QC.arbitrary
                          return $ QFor name e),
                      (do e <- QC.arbitrary
                          return $ QIf e)]

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
printExpression (Oper op e1 e2) = (printExpression e1) ++ (printOperator op) ++ (printExpression e2)
printExpression (Not e1) = "not " ++ (parens e1)
printExpression (Call name args) = name ++ (parens' (print'' args True))
printExpression (List elems) = (brackets (print'' elems True))
printExpression (Compr exp quals) = "[" ++ (parens exp) ++ (print''' quals True) ++ "]"

printQualifier :: Qual -> String
printQualifier (QFor name exp) = "for " ++ name ++ (parens exp)
printQualifier (QIf exp) = "in " ++ (parens exp)

printStatement :: Stmt -> String
printStatement (SDef name exp) = name ++ "=" ++ (printExpression exp)
printStatement (SExp exp) = (printExpression exp)

-- printProgram :: Program -> String
-- printProgram (s:p) = (printStatement s) ++ ";" ++ (printProgram p)