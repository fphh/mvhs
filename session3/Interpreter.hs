

module Interpreter where

import qualified Data.Map as Map
import Data.Map (Map)

data Expr a =
     Const a
     | Var String
     | Minus (Expr a)
     | Plus (Expr a) (Expr a)
     | Mult (Expr a) (Expr a)
     deriving (Show)

pretty :: (Show a) => Expr a -> String
pretty (Const x) = show x
pretty (Var str) = str
pretty (Minus x) = "-" ++ pretty x
pretty (Plus x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"
pretty (Mult x y) ="(" ++ pretty x ++ " * " ++ pretty y ++ ")"


x = Var "x"
y = Var "y"
z = Var "z"

_1:_2:_3:_4:_5:_ = map Const [1..]

(.+) :: Expr a -> Expr a -> Expr a
(.+) = Plus

infixl 6 .+


(.-) :: Expr a -> Expr a -> Expr a
(.-) = \x y -> Plus x (Minus y)

infixl 6 .-


(.*) :: Expr a -> Expr a -> Expr a
(.*) = Mult

infixl 7 .*

type Env a = Map String a

interpret :: (Num a) => Env a -> Expr a -> a
interpret _ (Const x) = x
interpret ms (Var v) =
  case Map.lookup v ms of
       Nothing -> error $ "Variable " ++ v ++ " not found"
       Just x -> x
interpret ms (Minus x) = -(interpret ms x)
interpret ms (Plus x y) = interpret ms x + interpret ms y
interpret ms (Mult x y) = interpret ms x * interpret ms y


env :: Env Integer
env = Map.fromList [("x", 7), ("y", 10)]

expr :: Expr Integer
expr = x .+ y .* _3

test :: Bool
test = interpret env expr == 37