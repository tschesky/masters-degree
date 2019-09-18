import BoaAST
import BoaInterp
import TestTypes

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.QuickCheck.Monadic as QCM
import qualified Test.Tasty.QuickCheck as QC
import Data.Either

-- Remove later
import Data.List
import Data.Ord
import System.Environment

-- To enable verbose options:
-- setEnv "TASTY_QUICKCHECK_VERBOSE" "TRUE"

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "All tests" [unitTst, propertyTst]

unitTst :: TestTree
unitTst = testGroup "UnitTests" [truthyTst, eqTst, outputTest]

-- Test truthy
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


-- Teste edge cases foor equlality operators
eqTst = testGroup "Test comparison opeartors in operate"
    [testCase "True == True" 
        (assertBool "" (TrueVal == (Data.Either.fromRight (FalseVal) (operate Eq TrueVal TrueVal)))),
    testCase "False == False"
        (assertBool "" (TrueVal == (Data.Either.fromRight (FalseVal) (operate Eq FalseVal FalseVal)))),
    testCase "True == False"
        (assertBool "" (FalseVal == (Data.Either.fromRight (TrueVal) (operate Eq TrueVal FalseVal)))),
    testCase "[] == False"
        (assertBool "" (FalseVal == (Data.Either.fromRight (TrueVal) (operate Eq (ListVal []) FalseVal)))),
    testCase "[] == []"
        (assertBool "" (TrueVal == (Data.Either.fromRight (FalseVal) (operate Eq (ListVal []) (ListVal []))))),
    testCase "-10 < 0 "
        (assertBool "" (TrueVal == (Data.Either.fromRight (FalseVal) (operate Less (IntVal (-10)) (IntVal 0))))),
    testCase "-100 < -10"
        (assertBool "" (TrueVal == (Data.Either.fromRight (FalseVal) (operate Less (IntVal (-100)) (IntVal (-10)))))),
    testCase "-100 > 100"
        (assertBool "" (FalseVal == (Data.Either.fromRight (TrueVal) (operate Greater (IntVal (-100)) (IntVal 100))))),
    testCase "0 > 0"
        (assertBool "" (FalseVal == (Data.Either.fromRight (TrueVal) (operate Greater (IntVal 0) (IntVal 0)))))
    ]

-- Test apply -> print -> output
outputTest = testGroup "a"
    [testCase "output NoneVal"
        (assertBool "" (["None"] == snd (runComp (apply "print" [NoneVal]) []))),
    testCase "output TrueVal"
        (assertBool "" (["True"] == snd (runComp (apply "print" [TrueVal]) []))),
    testCase "output FalseVal"
        (assertBool "" (["False"] == snd (runComp (apply "print" [FalseVal]) []))),
    testCase "output int 0"
        (assertBool "" (["0"] == snd (runComp (apply "print" [IntVal 0]) []))),
    testCase "output int -100"
        (assertBool "" (["-100"] == snd (runComp (apply "print" [IntVal (-100)]) []))),
    testCase "output []"
        (assertBool "" (["[]"] == snd (runComp (apply "print" [ListVal []]) []))),
    testCase "output [1, 2, 3]"
        (assertBool "" (["[1, 2, 3]"] == snd (runComp (apply "print" [ListVal [IntVal 1, IntVal 2, IntVal 3]]) []))),
    testCase "output [NoneVal]"
        (assertBool "" (["None"] == snd (runComp (apply "print" [NoneVal]) [])))
    ]

-- Proprety tests
propertyTst :: TestTree
propertyTst = testGroup "Property Tests" [prop_com,
                                          prop_ass, 
                                          prop_apply_range,
                                          prop_in_op,
                                          prop_look_withBinding,
                                          prop_list_compr]

prop_com = testGroup "Test commutative property"
    [ QC.testProperty "Commutative property of Plus, Mul and Eq" $
          \(CommOp o)  a b -> operate o (IntVal a) (IntVal b) == operate o (IntVal b) (IntVal a)]

prop_ass = testGroup "Test associative property"
    [ QC.testProperty "Associative property of Plus, Mul" $ testAssociative
    ]

prop_apply_range = testGroup "Test range function properties"
    [ QC.testProperty "Size of the generated list should be the same as (start-end)/step" $ testLength
    ]

prop_list_compr = testGroup "Test list conprehension properties"
    [ QC.testProperty "Identity comprehension with operator plus" $ testIdentityComprehensionPlus,
      QC.testProperty "Identity comprehension with operator minus" $ testIdentityComprehensionTimes
    ]

-- List returned list should always have the size equal to rouded (start-end)/step, except for a corner case where we return a one element list - step > (end - start)
testLength start end (QC.NonZero step) = (do list <- apply "range" [IntVal start, IntVal end, (IntVal step)]
                                             case list of
                                                (ListVal listVal') -> return $ length listVal'
                                                _                  -> return (-1))
                                          ==
                                            if ((step > 0) && (start >= end)) || ((step < 0) && (start <= end)) then createComp 0
                                                else (if (abs step > abs(end - start)) then createComp 1 else createComp $  ceiling $ abs (fromIntegral (end - start)/ fromIntegral step))

-- Certain operators should have the associative property
testAssociative (AssOp o) a b c = (do ab <- (operate o (IntVal a) (IntVal b))
                                      abc <- (operate o ab (IntVal c))
                                      return abc)
                                  ==
                                    (do bc <- (operate o (IntVal b) (IntVal c))
                                        abc <- (operate o (IntVal a) bc)
                                        return abc)

-- Operate on operator In should always retun the same value as elem function from Haskell
prop_in_op = testGroup "Test In operator property"
                [ QC.testProperty "Test In operator property" $
                    (\a (LV list@(ListVal b)) -> let inVal = (operate In a list) in
                                                case inVal of 
                                                    (Right v) -> truthy(v) == (a `elem` b)
                                                    _         -> False)
                ]

-- Binding a variable within an environment and then looking it up should provide the original value that was bound to it
prop_look_withBinding = testGroup "test look and withBinding"
                        [ QC.testProperty "test look and withBinding" $
                            (\vname val -> withBinding vname val (do a <- look vname
                                                                     return (a == val)) 
                                                   
                                            == 
                                            (return True))
                        ]

-- Calling a comprehension with identity element of plus should result in the exact same list
testIdentityComprehensionPlus start end (QC.NonZero step) = do inputList <- eval (Call "range" [Const $ IntVal start, Const $ IntVal end, Const $ IntVal step]) 
                                                               tmpList <- eval (Compr (Oper Plus (Const $ IntVal 0) (Var "x")) [QFor "x" (Const inputList)])
                                                               return (inputList == tmpList)
                                                            == 
                                                               (return True)

-- Calling a comprehension with identity element of times should result in the exact same list
testIdentityComprehensionTimes start end (QC.NonZero step) = do inputList <- eval (Call "range" [Const $ IntVal start, Const $ IntVal end, Const $ IntVal step]) 
                                                                tmpList <- eval (Compr (Oper Times (Const $ IntVal 1) (Var "x")) [QFor "x" (Const inputList)])
                                                                return (inputList == tmpList)
                                                             == 
                                                                (return True)



---- Commutative  - Plus, Times
---- Associative  - 
---- Why why skip distrubutative
---- Identity