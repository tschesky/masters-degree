data Exp =
    Cst Integer
    | Add Exp Exp
    | Sub Exp Exp
    | Mul Exp Exp
    | Div Exp Exp
    | Pow Exp Exp
    | If {test, yes, no :: Exp}
    | Var VName
    | Let {var :: VName, aux, body :: Exp}
    | Sum {var :: VName, from, to, body :: Exp}
type VName = String

showExp :: Exp -> String
showExp (Cst a) = show a
showExp (Add a b) = "(" ++ showExp a ++ "+" ++ showExp b ++ ")"
showExp (Sub a b) = "(" ++ showExp a ++ "-" ++ showExp b ++ ")"
showExp (Mul a b) = "(" ++ showExp a ++ "*" ++ showExp b ++ ")"
showExp (Div a b) = "(" ++ showExp a ++ "/" ++ showExp b ++ ")"
showExp (Pow a b) = "(" ++ showExp a ++ "^" ++ showExp b ++ ")"
showExp _ = error "Cannot show the expression, one of unsupported Exp values has been passed!"

evalSimple :: Exp -> Integer
evalSimple (Cst a) = a
evalSimple (Add a b) = (evalSimple a + evalSimple b)
evalSimple (Sub a b) = (evalSimple a - evalSimple b)
evalSimple (Mul a b) = (evalSimple a * evalSimple b)
evalSimple (Div a b) = (evalSimple a  `div` evalSimple b)
evalSimple (Pow a b) = (evalSimple a ^ evalSimple b)
evalSimple _ = error "Cannot evaluate the expression, one of unsupported Exp values has been passed!"

main :: IO ()
main = do print $ showExp ( Mul (Cst 2) (Add (Cst 3) (Cst 4)) )
          print $ showExp ( Add (Mul (Cst 2) (Cst 3)) (Cst 4) )
          -- print $ showExp ( Var "foo" )
          print $ evalSimple ( Mul (Cst 2) (Add (Cst 3) (Cst 4)) )
          print $ evalSimple ( Add (Mul (Cst 2) (Cst 3)) (Cst 4) )