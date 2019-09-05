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
showExp _ = error "Cannot show the expression, one of unsupported Exp values has been passed!"

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
   | evalB == 0 = 1
   | otherwise  = error "Negative exponent!"
   where  evalA = evalSimple a
          evalB = evalSimple b
evalSimple _ = error "Cannot evaluate the expression, one of unsupported Exp values has been passed!"

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
evalFull (Cst a) _ = a
evalFull (Add a b) env = (evalFull a env + evalFull b env)
evalFull (Sub a b) env = (evalFull a env - evalFull b env)
evalFull (Mul a b) env = (evalFull a env * evalFull b env)
evalFull (Div a b) env
  | evalB == 0 = error "Attempted division by 0"
  | otherwise  = (evalFull a env  `div` evalB)
  where evalB = evalFull b env
evalFull (Pow a b) env
   | evalB > 0  = (evalFull a env ^ evalB)
   | evalB == 0 = 1
   | otherwise  = error "Negative exponent!"
   where evalB = evalFull b env

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (If test yes no) env = case (evalErr test env) of
  (Left error) -> Left error
  (Right value) -> case value of
    0 -> evalErr no env
    _ -> evalErr yes env
evalErr (Var name) env = case (env name) of
  (Just a)  -> Right a
  Nothing -> Left (EBadVar name)
evalErr (Let var aux body) env = case (evalErr aux env) of
    (Left error) -> Left error
    (Right value) -> evalErr body (extendEnv var value env)
evalErr (Sum var from to body) env = case (eFrom, eTo) of
    ((Left error), (_)) -> Left error
    ((__), (Left error)) -> Left error
    ((Right fromValue), (Right toValue)) -> case (compare fromValue toValue) of
      GT -> Right 0
      EQ -> evalErr (Let var (Cst fromValue) body) env
      LT -> case (left, right) of
        ((Left error), _) -> Left error
        (_, (Left error)) -> Left error
        ((Right lValue), (Right rValue)) -> Right (lValue + rValue)
        where left  = (evalErr (Sum var (Add (Cst fromValue) (Cst 1)) (Cst toValue) body) env)
              right = (evalErr (Let var (Cst fromValue) body) env)
    where eFrom = evalErr from env
          eTo   = evalErr to env
evalErr (Cst a) _ = Right a
evalErr (Add a b) env = case (left, right) of
  ((Left error), (_)) -> Left error
  ((__), (Left error)) -> Left error
  ((Right lValue), (Right rValue)) -> Right (lValue + rValue)
  where left  = evalErr a env
        right = evalErr b env
evalErr (Sub a b) env = case (left, right) of
  ((Left error), (_)) -> Left error
  ((__), (Left error)) -> Left error
  ((Right lValue), (Right rValue)) -> Right (lValue - rValue)
  where left  = evalErr a env
        right = evalErr b env
evalErr (Mul a b) env = case (left, right) of
  ((Left error), (_)) -> Left error
  ((__), (Left error)) -> Left error
  ((Right lValue), (Right rValue)) -> Right (lValue * rValue)
  where left  = evalErr a env
        right = evalErr b env
evalErr (Div a b) env = case (left, right) of
  ((Left error), (_)) -> Left error
  ((__), (Left error)) -> Left error
  ((Right lValue), (Right rValue)) -> case rValue of 
    0 -> Left EDivZero
    _ -> Right (lValue `div` rValue)
  where left  = evalErr a env
        right = evalErr b env
evalErr (Pow a b) env = case (left, right) of
  ((Left error), (_)) -> Left error
  ((__), (Left error)) -> Left error
  ((Right lValue), (Right rValue)) -> case (compare rValue 0) of
    GT -> Right (lValue ^ rValue)
    EQ -> Right 1
    LT -> Left ENegPower
  where left  = evalErr a env
        right = evalErr b env

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
