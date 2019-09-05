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
evalSimple (Div a b)
  | evalB == 0 = error "Attempted division by 0"
  | otherwise  = (evalSimple a  `div` evalB)
  where evalB = evalSimple b
evalSimple (Pow a b)
   | evalB > 0  = (evalSimple a ^ evalB)
   | evalB == 0 = 1
   | otherwise  = error "Negative exponent!"
   where evalB = evalSimple b
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
evalFull exp _ = evalSimple exp
-- Add suppport simple arithmethics for all expression types

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr = undefined

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
