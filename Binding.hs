module Binding where

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda 
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

-- Substitute macros using the context
substituteMacros :: Context -> Lambda -> Either String Lambda
substituteMacros ctx (Var x) = Right (Var x)
substituteMacros ctx (Abs x e) = do
    e' <- substituteMacros ctx e
    Right (Abs x e')
substituteMacros ctx (App e1 e2) = do
    e1' <- substituteMacros ctx e1
    e2' <- substituteMacros ctx e2
    Right (App e1' e2')
substituteMacros ctx (Macro m) = case lookup m ctx of
    Just e -> Right e
    Nothing -> Left ("Undefined macro: " ++ m)

-- 3.1.
simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx step expr = do
    expr' <- substituteMacros ctx expr
    Right (simplify step expr')

normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
