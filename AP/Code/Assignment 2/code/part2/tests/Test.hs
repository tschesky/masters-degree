-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.QuickCheck.Monadic as QCM
import qualified Test.Tasty.QuickCheck as QC

-- Remove later
import Data.List
import Data.Ord
import System.Environment

main :: IO ()
main = do setEnv "TASTY_QUICKCHECK_VERBOSE" "TRUE"
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
propertyTst = testGroup "Test properties of arithmetic operators" [prop_com, prop_ass, prop_apply_range]

-- commutative
newtype CommOperators = CommOp Op
    deriving (Eq, Show)
instance QC.Arbitrary CommOperators where
 arbitrary = fmap CommOp (QC.elements [Plus, Times, Eq])

newtype AssOperators = AssOp Op
 deriving (Eq, Show)
instance QC.Arbitrary AssOperators where
 arbitrary = fmap AssOp (QC.elements [Plus, Times])

prop_com = testGroup "Test commutative property"
    [ QC.testProperty "Commutative property of Plus, Mul and Eq" $
          \(CommOp o)  a b -> operate o (IntVal a) (IntVal b) == operate o (IntVal b) (IntVal a)]

prop_ass = testGroup "Test commutative property"
    [ QC.testProperty "Associative property of Plus, Mul" $ testAssociative
    ]

prop_apply_range = testGroup "Test range function properties"
    [ QC.testProperty "Size of the generated list" $ testLength
    ]

testLength start end (QC.NonZero step) = (do list <- apply "range" [IntVal start, IntVal end, (IntVal step)]
                                             case list of
                                                (ListVal listVal') -> return $ length listVal'
                                                _                  -> return (-1))
                                          ==
                                            if ((step > 0) && (start >= end)) || ((step < 0) && (start <= end)) then createComp 0
                                                else (if (abs step > abs(end - start)) then createComp 1 else createComp $  ceiling $ abs (fromIntegral (end - start)/ fromIntegral step))

-- testAssociative :: AssOp -> Value -> Value -> Value -> 
testAssociative (AssOp o) a b c = (do ab <- (operate o (IntVal a) (IntVal b))
                                      abc <- (operate o ab (IntVal c))
                                      return abc)
                                  ==
                                    (do bc <- (operate o (IntVal b) (IntVal c))
                                        abc <- (operate o (IntVal a) bc)
                                        return abc)

---- Commutative  - Plus, Times
---- Associative  - 
---- Why why skip distrubutative
---- Identity

createComp :: a -> Comp a
createComp a = Comp (\_e -> (Right a, []))

instance (Eq a) => Eq (Comp a) where
    (Comp a) == (Comp b) = ((a []) == (b []))