-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit
import Test.QuickCheck.Monadic
import Test.Tasty.QuickCheck as QC

-- Remove later
import Data.List
import Data.Ord
import System.Environment

main :: IO ()
main = do setEnv "TASTY_QUICKCHECK_VERBOSE" "FALSE"
          defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Tests" [unitTst, propertyTst]

unitTst :: TestTree
unitTst = testGroup "UnitTests" [truthyTst]

truthyTst = testGroup "Testing truth values"
    [testCase "truthy Nothing"
        (assertBool "" (False == truthy(NoneVal))),
     testCase "truthy False "
        (assertBool "" (False == truthy(FalseVal))),
    testCase "truthy 9999 "
        (assertBool "" (True == truthy(IntVal 9999))),
    testCase "truthy 0 "
        (assertBool "" (False == truthy(IntVal 0))),
    testCase "truthy \"Looooooong string\" "
        (assertBool "" (True == truthy(StringVal "Looooooong string"))),
    testCase "truthy \"\""
        (assertBool "" (False == truthy(StringVal ""))),
    testCase "truthy []"
        (assertBool "" (False == truthy(ListVal []))),
    testCase "truthy [1, 2]"
        (assertBool "" (True == truthy(ListVal [IntVal 1, IntVal 2]))),
    testCase "truthy [False]"
        (assertBool "" (True == truthy(ListVal [FalseVal]))),
    testCase "truthy [True, False]]"
        (assertBool "" (True == truthy(ListVal [TrueVal, FalseVal])))]

propertyTst :: TestTree
propertyTst = testGroup "Test properties of arithmetic operators" [prop_com]

-- commutative
newtype CommOperators = CommOp Op
    deriving (Eq, Show)
instance QC.Arbitrary CommOperators where
 arbitrary = fmap CommOp (QC.elements [Plus, Times, Eq])

-- newtype AssOperators = AssOp Op
--  deriving (Eq, Show)
-- instance QC.Arbitrary AssOperators where
-- arbitrary = fmap AssOp (QC.elements [Plus, Times])


prop_com = testGroup "Test commutative property"
    [ QC.testProperty "Commutative property of Plus, Mul and Eq" $
          \(CommOp o)  a b -> operate o (IntVal a) (IntVal b) == operate o (IntVal b) (IntVal a)]

-- prop_ass = testGroup "Test commutative property"
-- [ QC.testProperty "Associative property of Plus, Mul and Eq" $
--     \(AssOp o)  a b -> operate o (IntVal a) (IntVal b) == operate o (IntVal b) (IntVal a)]
      

-- Unit tests for simple auxiliaries
-- Property testing for operate
---- Commutative  - Plus, Times
---- Associative  - 
---- Why why skip distrubutative
---- Identity 

-- expTests = testGroup "testing expression properties"
--     [QC.testProperty "a + b = b + a" $ \a b -> a + b == b + a]

-- prop_com_add a b = operate Plus a b == operate Plus b a

-- propertyTst :: TestTree
-- propertyTst = testGroup "PropertyTests" [simpleProp]

-- simpleProp = testGroup "Example property test" [QC.testProperty "blahblah" $ prop_compST]

-- prop_compST a = monadicST $ do
--   value  <- run (Comp a  >>= (+3))
--   Test.QuickCheck.Monadic.assert $ 8 == value