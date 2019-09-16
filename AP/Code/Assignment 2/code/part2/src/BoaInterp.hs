-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

instance Monad Comp where
  return a = Comp (\_e -> (Right a, []))
  m >>= f  = Comp (\e -> let (firstA, firstSL) = (runComp m) e in
                          let a1 = firstA >>= (\a1' -> Right a1') in --extract a1 from m 
                            case a1 of
                              (Left err) -> ((Left err), firstSL)
                              (Right val) -> let (secondA, secondSL) = runComp (f val) e in -- apply f to a1, then evaluate the returned a (secondA)
                                (secondA >>= (\a2 -> Right a2), firstSL ++ secondSL)) -- construct tuple containing Either
                            
                       

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort e = Comp (\_env -> (Left e, []))

look :: VName -> Comp Value
look name = Comp (\env -> case (lookup name env) of
                            (Just a) -> (Right a, [])
                            Nothing -> (Left (EBadVar name), []))                    

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding name val m = Comp (\env -> (runComp m) ((name, val) : env))

output :: String -> Comp ()
output s = Comp (\_env -> (Right (), [s]))

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy NoneVal = False
truthy FalseVal = False
truthy (IntVal 0) = False 
truthy (StringVal "") = False
truthy (ListVal []) = False
truthy _ = True

operate :: Op -> Value -> Value -> Either String Value
-- arithmetic ops
operate Plus (IntVal v1) (IntVal v2) = Right $ IntVal $ v1 + v2
operate Plus _ _ = Left "Plus operation only defined on two IntVals"
operate Minus (IntVal v1) (IntVal v2) = Right $ IntVal $ v1 - v2
operate Minus _ _ = Left "Minus operation only defined on two IntVals"

operate Times (IntVal v1) (IntVal v2) = Right $ IntVal $ v1 * v2
operate Times _ _ = Left "Times operation only defined on two IntVals" 
operate Div (IntVal v1) (IntVal v2)
  | v2 /= 0 = Right $ IntVal $ v1 `div` v2
  | otherwise = Left "Attempted division by zero"
operate Div _ _ = Left "Div operation only defined on two IntVals"
operate Mod (IntVal v1) (IntVal v2)
  | v2 /= 0 = Right $ IntVal $ v1 `mod` v2
  | otherwise = Left "Attempted to execute x modulo zero"
operate Mod _ _ = Left "Mod operation only defined on two IntVals"
-- equality ops
operate Eq v1 v2 = Right $ if (v1 == v2) then TrueVal else FalseVal
operate Less (IntVal v1) (IntVal v2) = Right $ if (v1 < v2) then TrueVal else FalseVal
operate Less _ _ = Left "Less operation only defined on two Intvals"
operate Greater (IntVal v1) (IntVal v2) = Right $ if (v1 > v2) then TrueVal else FalseVal
operate Greater _ _ = Left "Greater operation only defined on two Intvals"
-- In op
operate In v1 (ListVal v2) = Right $ if v1 `elem` v2 then TrueVal else FalseVal
operate In _ _ = Left "In operator takes only Lists as second argument!"


apply :: FName -> [Value] -> Comp Value 
apply "range" ((IntVal a):(IntVal b):(IntVal step):rest) 
  | rest == [] = return (ListVal [(IntVal x) | x <- [a, (a+step)..(b-(signum step))]])
  | step == 0 = abort (EBadArg "range function called with zero step")
  | otherwise = abort (EBadArg "range function called with >3 arguments.")
apply "range" ((IntVal a):(IntVal b):rest)
  | rest == [] = return (ListVal [(IntVal x) | x <- [a, (a+1)..(b-1)]])
  | otherwise = abort (EBadArg "range called with non-integer arguments.") 
apply "range" ((IntVal b):rest)
  | rest == [] = return (ListVal [(IntVal x) | x <- [0, 1..(b-1)]])
  | otherwise = abort (EBadArg "range called with non-integer arguments.") 
apply "range" [] = abort (EBadArg "range called with zero arguments."  )
apply "range" _ = abort (EBadArg "range called with non-integer arguments.")

apply "print" x = do output (print' x False)
                     return NoneVal 
apply fname _ = abort (EBadFun fname)

-- Bool indicates if value is in list
print' :: [Value] -> Bool -> String
print' [] _ = ""
-- print' ((StringVal s):xs) il
--   | xs == [] = if il then "'" ++ s ++ "'" else s
--   | otherwise = (if il then "'" ++ s ++ "', " else s ++ " ") ++ (print' xs il)
print' (x:xs) il
  | xs == [] = (printVal x)
  | otherwise = (if il then (printVal x) ++ ", " else (printVal x) ++ " ") ++ (print' xs il)

printVal :: Value -> String
printVal NoneVal = "None"
printVal TrueVal = "True"
printVal FalseVal = "False"
printVal (IntVal x) = show x
printVal (StringVal x) = x
printVal (ListVal x) = "[" ++ (print' x True) ++ "]" -- this should not happen though

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval (Const val) = return val
eval (Var vname) = look vname
eval (Oper op e1 e2) = do v1 <- eval e1
                          v2 <- eval e2
                          case (operate op v1 v2) of
                            (Left err) -> abort (EBadArg err)
                            (Right res) -> return res
eval (Not e) = do v <- eval e
                  if truthy v then return FalseVal else return TrueVal
eval (Call fname arge) = do argv <- eval (List arge)
                            case argv of
                              (ListVal l) -> apply fname l
                              _ -> error "this code should be unreachable."
eval (List []) = return (ListVal [])
eval (List (e1:es)) = do v1 <- eval e1
                         vs <- eval (List es)
                         case vs of 
                          (ListVal l) -> return (ListVal (v1 : l))
                          _ -> error "this code should be unreachable."

eval (Compr e []) = do lv <- eval e
                       return (ListVal [lv])
eval (Compr _ ((QFor _ (Const (ListVal []))):_)) = return (ListVal [])
eval (Compr e ((QFor vname (Const (ListVal (l:ls)))):qs)) = do res1 <- withBinding vname l (eval (Compr e qs))
                                                               res2 <- eval (Compr e ((QFor vname (Const (ListVal ls))):qs))
                                                               case res2 of 
                                                                (ListVal rl2) -> case res1 of
                                                                                  (ListVal rl1) -> return (ListVal (rl1 ++ rl2))
                                                                                  _ -> error "Compr did not return a list. Impl error!" -- return (ListVal (res1 : rl2))
                                                                _ -> error "Compr did not return a list. Impl error!" -- QFor always returns a ListVal                                                                                         
-- evaluate list and call the eval with const listval, see above
eval (Compr e ((QFor vname fe):qs)) = do fv <- eval fe
                                         case fv of
                                           (ListVal l) -> eval (Compr e ((QFor vname (Const (ListVal l))):qs))
                                           _ -> abort (EBadArg "2nd. argument of Compr should be a list.")
eval (Compr e ((QIf ie):qs)) = do iv <- eval ie
                                  if (truthy iv) then eval (Compr e qs) else return (ListVal [])

exec :: Program -> Comp ()
exec [] = return ()
exec ((SDef vname e):sts) = do v <- eval e
                               withBinding vname v (exec sts) 
exec ((SExp e):sts) =       do eval e
                               exec sts


execute :: Program -> ([String], Maybe RunError)
execute p = case err of
             (Left err) -> (res, Just err)
             (Right _) -> (res, Nothing)
  where (err, res) = runComp (exec p) []
               
