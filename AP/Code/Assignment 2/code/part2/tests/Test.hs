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

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Tests" [unitTst, propertyTst]

unitTst :: TestTree
unitTst = testGroup "UnitTests" [truthyTst, equalityUnitTst]

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

expTests = testGroup "Example"
    [ QC.testProperty "Plus a b == Plus b a" $
          \a b -> operate Plus (IntVal a) (IntVal b) == operate Plus (IntVal b) (IntVal a)]

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