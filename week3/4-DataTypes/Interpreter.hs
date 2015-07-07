import qualified Data.Map.Strict as Map

type Variable = String
type Value    = Int

type Variables = Map.Map Variable Value

data Oper = Plus
          | Minus
          | Lt
          | Gt
          | Mod
          | Div
          | Le
          | Ge
          | Eql
  deriving (Show, Eq)

data Expr = Var Variable
          | Val Value
          | Op  Expr Oper Expr
  deriving (Show, Eq)

data Statement = Assign Variable Expr
               | Incr Variable
               | Decr Variable
               | For Statement Expr Statement Statement
               | If Expr Statement
  deriving (Show, Eq)

data State = State [Statement] Variables
  deriving (Show, Eq)

-- Evaluate an expression
evalE :: Expr -> Variables -> Maybe Value
evalE (Val x) _ = Just x
evalE (Var k) a = Map.lookup k a
evalE (Op x op y) a | op == Plus  = Just $ x1 + y1
                    | op == Minus = Just $ x1 - y1
                    | op == Mod   = Just $ x1 `mod` y1
                    | op == Div   = Just $ x1 `div` y1
                    | op == Lt    = Just $ if x1 < y1 then 1 else 0
                    | op == Gt    = Just $ if x1 > y1 then 1 else 0
                    | op == Le    = Just $ if x1 <= y1 then 1 else 0
                    | op == Ge    = Just $ if x1 >= y1 then 1 else 0
                    | op == Eql   = Just $ if x1 == y1 then 1 else 0
                      where Just x1 = evalE x a
                            Just y1 = evalE y a


-- Execute the current statement and update the state
execS :: State -> State
execS (State ((Assign var expr):xs) a) = State xs $ Map.insert var x a
  where Just x = evalE expr a
execS (State ((Incr var):xs) a) = State xs $ Map.update incr var a
  where incr = \y -> Just (y+1)
execS (State ((Decr var):xs) a) = State xs $ Map.update decr var a
  where decr = \y -> Just (y-1)
-- execS (State ((For init cond aftt body):xs) a) = State xs $ 

execS (State ((If expr stat):xs) a) = if evalE expr == 1
                                        then State (stat:xs) a
                                        else State xs a
