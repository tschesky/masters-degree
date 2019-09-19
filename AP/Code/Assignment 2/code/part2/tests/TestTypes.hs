module TestTypes where

import qualified Test.Tasty.QuickCheck as QC
import BoaAST
import BoaInterp
import Control.Monad

-- Arbitrary data type for commutative operators
newtype CommOperators = CommOp Op
    deriving (Eq, Show)
instance QC.Arbitrary CommOperators where
 arbitrary = fmap CommOp (QC.elements [Plus, Times, Eq])

-- Arbitrary data type for associative operators
newtype AssOperators = AssOp Op
 deriving (Eq, Show)
instance QC.Arbitrary AssOperators where
 arbitrary = fmap AssOp (QC.elements [Plus, Times])

-- Generators for value data type
newtype ListValue = LV Value deriving (Eq, Show)
instance QC.Arbitrary ListValue where
    arbitrary = QC.sized listVal

instance QC.Arbitrary Value where
    arbitrary = sizedVal

sizedVal = QC.sized valN
-- valN :: Int -> Gen a
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

-- Helper functions for Moand coparison
createComp :: a -> Comp a
createComp a = Comp (\_e -> (Right a, []))

instance (Eq a) => Eq (Comp a) where
    (Comp a) == (Comp b) = ((a []) == (b []))