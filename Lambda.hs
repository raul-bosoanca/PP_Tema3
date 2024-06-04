module Lambda where

import Data.List (nub, (\\))

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.
vars :: Lambda -> [String]
vars (Var x) = [x]
vars (Abs x e) = nub (x : vars e)
vars (App e1 e2) = nub ((vars e1) ++ (vars e2))

-- 1.2.
freeVars :: Lambda -> [String]
freeVars expr = nub (freeVarsAux expr [])

freeVarsAux :: Lambda -> [String] -> [String]
freeVarsAux (Var x) bounded_vars
  | x `elem` bounded_vars = []
  | otherwise      = [x]
freeVarsAux (App e1 e2) bounded_vars = freeVarsAux e1 bounded_vars ++ freeVarsAux e2 bounded_vars
freeVarsAux (Abs x e) bounded_vars = freeVarsAux e (x : bounded_vars) 

-- 1.3.
generateStrings :: [String]
generateStrings = concatMap genStrings [1..7]
  where
    genStrings n = sequence (replicate n ['a'..'z'])

newVar :: [String] -> String
newVar l = newVarAux generateStrings l


newVarAux :: [String] -> [String] -> String
newVarAux [] xs = "aa"
newVarAux (x : xs1) xs2
  | x `notElem` xs2 = x
  | otherwise = newVarAux xs1 xs2

-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm (App (Abs x e1) e2) = False
isNormalForm (Var _) = True
isNormalForm (App e1 e2) = (isNormalForm e1) && (isNormalForm e2)
isNormalForm (Abs x e) = isNormalForm e 

-- 1.5.
substitute :: String -> String -> Lambda -> Lambda
substitute old new (Var x)
  | x == old = Var new
  | otherwise = Var x
substitute old new (Abs x e)
  | x == old = Abs new (substitute old new e)
  | otherwise = Abs x (substitute old new e)
substitute old new (App e1 e2) = App (substitute old new e1) (substitute old new e2)

reduce :: String -> Lambda -> Lambda -> Lambda
reduce x (Var y) e2 
  | x == y = e2
  | otherwise = Var y
reduce x (Abs y e1) e2
  | x == y = Abs y e1
  | y `elem` freeVars e2 = reduce x (Abs (newVar (freeVars e2)) (substitute y (newVar (freeVars e2)) e1)) e2
  | otherwise = Abs y (reduce x e1 e2)
reduce x (App e11 e22) e2 = App (reduce x e11 e2) (reduce x e22 e2) 

-- 1.6.
normalStep :: Lambda -> Lambda
normalStep (App (Abs x e1) e2) = reduce x e1 e2
normalStep (App e1 e2)
  | isNormalForm e1 = App e1 (normalStep e2)
  | otherwise = App (normalStep e1) e2
normalStep (Abs x e) = Abs x (normalStep e)
normalStep (Var x) = Var x

-- 1.7.
applicativeStep :: Lambda -> Lambda
applicativeStep (App (Abs x e1) e2) 
  | isNormalForm e2 && isNormalForm e1 = reduce x e1 e2
  | isNormalForm e2  = (App (Abs x (applicativeStep e1)) e2)
  | otherwise = (App (Abs x e1) (applicativeStep e2))
applicativeStep (App e1 e2)
  | isNormalForm e1 = App (e1) (applicativeStep e2)
  | otherwise = App (applicativeStep e1) e2
applicativeStep (Abs x e) = Abs x (applicativeStep e)
applicativeStep (Var x) = Var x

-- 1.8.
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify step e
  | isNormalForm e = [e]
  | otherwise = e : simplify step (step e)

normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
