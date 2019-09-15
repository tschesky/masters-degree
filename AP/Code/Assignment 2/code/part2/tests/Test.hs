-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Stubby tests"
  [testCase "execute crash.boa" $
    execute crashAST @?= crashOut]
  where
    crashAST = [SExp (Call "print" [Oper Plus (Const (IntVal 2))
                                              (Const (IntVal 2))]),
                SExp (Var "hello")]
    crashOut = (["4"], Just (EBadVar "hello"))

