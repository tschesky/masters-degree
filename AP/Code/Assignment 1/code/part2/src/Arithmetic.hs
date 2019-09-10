-- This is a skeleton file for you to edit

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions

showExp :: Exp -> String
showExp (Cst a)
  | a < 0 = "(" ++ show a ++ ")"
  | otherwise = show a
showExp (Add a b) = "(" ++ showExp a ++ "+" ++ showExp b ++ ")"
showExp (Sub a b) = "(" ++ showExp a ++ "-" ++ showExp b ++ ")"
showExp (Mul a b) = "(" ++ showExp a ++ "*" ++ showExp b ++ ")"
showExp (Div a b) = "(" ++ showExp a ++ "/" ++ showExp b ++ ")"
showExp (Pow a b) = "(" ++ showExp a ++ "^" ++ showExp b ++ ")"
showExp _ = error "Cannot show the expression, an unsupported Exp values has been passed!"

evalSimple :: Exp -> Integer
evalSimple (Cst a) = a
evalSimple (Add a b) = (evalSimple a + evalSimple b)
evalSimple (Sub a b) = (evalSimple a - evalSimple b)
evalSimple (Mul a b) = (evalSimple a * evalSimple b)
evalSimple (Div a b)
  | evalB == 0 = error "Attempted division by 0"
  | otherwise  = (evalSimple a  `div` evalB)
  where evalB = evalSimple b
evalSimple (Pow a b)
   | evalB > 0  = (evalA ^ evalB)
   | evalB == 0 && evalA == 0 = 1
   | evalB == 0 && evalA /= 0 = 1
   | otherwise  = error "Negative exponent!"
   where  evalA = evalSimple a
          evalB = evalSimple b
evalSimple _ = error "Cannot evaluate the expression, an unsupported Exp values has been passed!"

extendEnv :: VName -> Integer -> Env -> Env
extendEnv name val env = (\v -> if v == name then (Just val) else (env v))

evalFull :: Exp -> Env -> Integer
evalFull (If test yes no) env
  | evalFull test env /= 0 = evalFull yes env
  | otherwise              = evalFull no env
evalFull (Var name) env = case (env name) of
  (Just a)  -> a
  Nothing -> error "Attepmted access to an unbound variable"
evalFull (Let var aux body) env = evalFull body (extendEnv var (evalFull aux env) env)
evalFull (Sum var from to body) env
  | eFrom > eTo  = 0
  | eFrom == eTo = evalFull (Let var (Cst eFrom) body) env
  | otherwise    = evalFull (Sum var (Cst (eFrom + 1)) (Cst eTo) body) env + evalFull (Let var (Cst eFrom) body) env
  where eFrom = evalFull from env
        eTo   = evalFull to env

-- simple functions
evalFull (Cst a) _ = a
evalFull (Add a b) env = (evalFull a env + evalFull b env)
evalFull (Sub a b) env = (evalFull a env - evalFull b env)
evalFull (Mul a b) env = (evalFull a env * evalFull b env)
evalFull (Div a b) env
  | evalB == 0 = error "Attempted division by 0"
  | otherwise  = (evalFull a env  `div` evalB)
  where evalB = evalFull b env
evalFull (Pow a b) env
  | evalB > 0  = (evalA ^ evalB)
  -- stupid thing to make haskell evaluate evalA.
  | evalB == 0 && evalA == 0 = 1
  | evalB == 0 && evalA /= 0 = 1
  | otherwise  = error "Negative exponent!"
  where evalA = evalFull a env
        evalB = evalFull b env


evalErr :: Exp -> Env -> Either ArithError Integer
-- IF
evalErr (If test yes no) env = case testRes of
                                (Left error) -> (Left error)
                                (Right t) -> if t /= 0 then evalErr yes env else evalErr no env
                                where testRes = evalErr test env
-- VARIABLE RESOLUTION
evalErr (Var name) env = case (env name) of
  (Just a)  -> (Right a)
  Nothing -> (Left (EBadVar name))
-- LET
evalErr (Let var aux body) env = case resAux of
                                  (Left error) -> (Left error)
                                  (Right res) -> evalErr body (extendEnv var res env)
                                  where resAux = evalErr aux env
-- SUM
evalErr (Sum var from to body) env =
  case result of
    (Left error) -> (Left error)
    (Right (eFrom, eTo)) -> case (compare eFrom eTo) of
      GT -> (Right 0)
      EQ -> evalErr (Let var (Cst eFrom) body) env
      LT -> case result of
                    (Left error) -> (Left error)
                    (Right (a, b)) -> (Right (a + b))
                    -- a = sum starting at next index, b is evaluation of body at current index
                    -- will be called recursively until from == to
                    where result = tryEval (Sum var (Cst (eFrom + 1)) (Cst eTo) body) env (Let var (Cst eFrom) body) env
  where result = tryEval from env to env

-- simple functions
evalErr (Cst a) _ = (Right a)
evalErr (Add a b) env = case res of
                          (Left error) -> (Left error)
                          (Right (resA, resB)) -> (Right (resA+resB))
                          where res = tryEval a env b env
evalErr (Sub a b) env = case res of
                          (Left error) -> (Left error)
                          (Right (resA, resB)) -> (Right (resA - resB))
                          where res = tryEval a env b env
evalErr (Mul a b) env = case res of
                          (Left error) -> (Left error)
                          (Right (resA, resB)) -> (Right (resA * resB))
                          where res = tryEval a env b env
evalErr (Div a b) env = case res of
                          (Left error) -> (Left error)
                          (Right (resA, resB)) -> if resB == 0 then (Left EDivZero)
                                                    else (Right (resA `div` resB))
                          where res = tryEval a env b env
evalErr (Pow a b) env = case res of
                          (Left error) -> (Left error)
                          (Right (resA, resB)) -> case (compare resB 0) of
                                                      GT -> (Right (resA ^ resB))
                                                      EQ -> (Right 1)
                                                      LT -> (Left ENegPower)
                          where res = tryEval a env b env
-- helper function that evaluates two expressions and returns both results, or the "first" error that occurs
tryEval :: Exp -> Env -> Exp -> Env -> Either ArithError (Integer, Integer)
tryEval a envA b envB = case (resA, resB) of
                          ((Left error), _) -> (Left error)
                          (_, (Left error)) -> (Left error)
                          ((Right a), (Right b)) -> (Right (a, b))
                          where resA = evalErr a envA
                                resB = evalErr b envB


-- evalErr :: Exp -> Env -> Either ArithError Integer
-- evalErr (If test yes no) env = case (evalErr test env) of
--   (Left error) -> Left error
--   (Right value) -> case value of
--     0 -> evalErr no env
--     _ -> evalErr yes env
-- evalErr (Var name) env = case (env name) of
--   (Just a)  -> Right a
--   Nothing -> Left (EBadVar name)
-- evalErr (Let var aux body) env = case (evalErr aux env) of
--     (Left error) -> Left error
--     (Right value) -> evalErr body (extendEnv var value env)
-- evalErr (Sum var from to body) env = case (eFrom, eTo) of
--     ((Left error), (_)) -> Left error
--     ((__), (Left error)) -> Left error
--     ((Right fromValue), (Right toValue)) -> case (compare fromValue toValue) of
--       GT -> Right 0
--       EQ -> evalErr (Let var (Cst fromValue) body) env
--       LT -> case (left, right) of
--         ((Left error), _) -> Left error
--         (_, (Left error)) -> Left error
--         ((Right lValue), (Right rValue)) -> Right (lValue + rValue)
--         where left  = (evalErr (Sum var (Add (Cst fromValue) (Cst 1)) (Cst toValue) body) env)
--               right = (evalErr (Let var (Cst fromValue) body) env)
--     where eFrom = evalErr from env
--           eTo   = evalErr to env
-- evalErr (Cst a) _ = Right a
-- evalErr (Add a b) env = case (left, right) of
--   ((Left error), (_)) -> Left error
--   ((__), (Left error)) -> Left error
--   ((Right lValue), (Right rValue)) -> Right (lValue + rValue)
--   where left  = evalErr a env
--         right = evalErr b env
-- evalErr (Sub a b) env = case (left, right) of
--   ((Left error), (_)) -> Left error
--   ((__), (Left error)) -> Left error
--   ((Right lValue), (Right rValue)) -> Right (lValue - rValue)
--   where left  = evalErr a env
--         right = evalErr b env
-- evalErr (Mul a b) env = case (left, right) of
--   ((Left error), (_)) -> Left error
--   ((__), (Left error)) -> Left error
--   ((Right lValue), (Right rValue)) -> Right (lValue * rValue)
--   where left  = evalErr a env
--         right = evalErr b env
-- evalErr (Div a b) env = case (left, right) of
--   ((Left error), (_)) -> Left error
--   ((__), (Left error)) -> Left error
--   ((Right lValue), (Right rValue)) -> case rValue of
--     0 -> Left EDivZero
--     _ -> Right (lValue `div` rValue)
--   where left  = evalErr a env
--         right = evalErr b env
-- evalErr (Pow a b) env = case (left, right) of
--   ((Left error), (_)) -> Left error
--   ((__), (Left error)) -> Left error
--   ((Right lValue), (Right rValue)) -> case (compare rValue 0) of
--     GT -> Right (lValue ^ rValue)
--     EQ -> Right 1
--     LT -> Left ENegPower
--   where left  = evalErr a env
--         right = evalErr b env

-- optional parts (if not attempted, leave them unmodified)

-- change ALL names to isLowerOrEqualThan...
isLowerThanPow :: Exp -> Bool
isLowerThanPow (Pow _ _) = False
isLowerThanPow _ = True

isLowerOrEqualPow :: Exp -> Bool
isLowerOrEqualPow (Pow _ _) = True
isLowerOrEqualPow exp = isLowerThanPow exp

isLowerThanMul :: Exp -> Bool
isLowerThanMul (Add _ _) = True
isLowerThanMul (Sub _ _) = True
isLowerThanMul (Cst _)   = True
isLowerThanMul _         = False

isLowerOrEqualMul :: Exp -> Bool
isLowerOrEqualMul (Mul _ _) = True
isLowerOrEqualMul (Div _ _) = True
isLowerOrEqualMul exp = isLowerThanMul exp

isLowerThanDiv :: Exp -> Bool 
isLowerThanDiv exp = isLowerThanMul exp

isLowerOrEqualDiv :: Exp -> Bool
isLowerOrEqualDiv exp = isLowerOrEqualMul exp

isLowerThanAdd :: Exp -> Bool
isLowerThanAdd _ = False

isLowerOrEqualAdd :: Exp -> Bool
isLowerOrEqualAdd (Add _ _) = True
isLowerOrEqualAdd (Sub _ _) = True
isLowerOrEqualAdd (Cst _) = True
isLowerOrEqualAdd exp = isLowerThanAdd exp

isLowerThanSub :: Exp -> Bool
isLowerThanSub _ = False

isLowerOrEqualSub :: Exp -> Bool
isLowerOrEqualSub exp = isLowerOrEqualAdd exp

printWithParentheses :: Exp -> Bool -> String
printWithParentheses (Cst a) True
  | a >= 0    = show a
  | otherwise = "(" ++ (show a) ++ ")"
printWithParentheses (Cst a) False = show a
printWithParentheses (Pow a b) True  = "(" ++ (printWithParentheses a (isLowerOrEqualPow a)) ++ "^" ++ (printWithParentheses b (isLowerThanPow b)) ++ ")"
printWithParentheses (Pow a b) False = (printWithParentheses a (isLowerOrEqualPow a)) ++ "^" ++ (printWithParentheses b (isLowerThanPow b))
printWithParentheses (Mul a b) True  = "(" ++ (printWithParentheses a (isLowerThanMul a)) ++ "*" ++ (printWithParentheses b (isLowerOrEqualMul b)) ++ ")"
printWithParentheses (Mul a b) False = (printWithParentheses a (isLowerThanMul a)) ++ "*" ++ (printWithParentheses b (isLowerOrEqualMul b))
printWithParentheses (Div a b) True  = "(" ++ (printWithParentheses a (isLowerThanDiv a)) ++ "/" ++ (printWithParentheses b (isLowerOrEqualDiv b)) ++ ")"
printWithParentheses (Div a b) False = (printWithParentheses a (isLowerThanDiv a)) ++ "/" ++ (printWithParentheses b (isLowerOrEqualDiv b))
printWithParentheses (Add a b) True  = "(" ++ (printWithParentheses a (isLowerThanAdd a)) ++ "+" ++ (printWithParentheses b (isLowerOrEqualAdd b)) ++ ")"
printWithParentheses (Add a b) False = (printWithParentheses a (isLowerThanAdd a)) ++ "+" ++ (printWithParentheses b (isLowerOrEqualAdd b))
printWithParentheses (Sub a b) True  = "(" ++ (printWithParentheses a (isLowerThanSub a)) ++ "-" ++ (printWithParentheses b (isLowerOrEqualSub b)) ++ ")"
printWithParentheses (Sub a b) False = (printWithParentheses a (isLowerThanSub a)) ++ "-" ++ (printWithParentheses b (isLowerOrEqualSub b))
printWithParentheses _ _= error "Cannot evaluate the expression, an unsupported Exp values has been passed!"

showCompact :: Exp -> String
showCompact (Cst a) = show a
showCompact exp = printWithParentheses exp False

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
