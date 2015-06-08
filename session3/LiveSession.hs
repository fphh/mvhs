
import qualified Data.Map as Map
import Data.Map (Map)

a = (*4)
b = (+3)



data N = N0 | N1 deriving (Show)
{-
(.+) :: N -> N -> N
N0 .+ N0 = N0
N0 .+ N1 = N1
N1 .+ N0 = N1
N1 .+ N1 = N0

infixl 6 .+


(.*) :: N -> N -> N
N0 .* N0 = N0
N0 .* N1 = N0
N1 .* N0 = N0
N1 .* N1 = N1

infixl 7 .*
-}

data Expr a = Const a
            | Var String
            | Minus (Expr a)
            | Plus (Expr a) (Expr a)
            | Mult (Expr a) (Expr a)
            deriving (Show)

x = Var "x"
y = Var "y"
z = Var "z"

_1 = Const 1
_2 = Const 2
_3 = Const 3

(.+) = Plus
(.*) = Mult

-- x .- y = Plus x (Minus y)

(.-) = \x -> Plus x . Minus

pretty :: (Show a) => Expr a -> String
pretty (Const x) = show x
pretty (Var x) = x
pretty (Minus x) = "-" ++ pretty x
pretty (Plus x y) = pretty x ++ "+" ++ pretty y
pretty (Mult x y) = pretty x ++ "*" ++ pretty y


type Env a = Map String a


env :: Env Integer
env = Map.fromList [("x", 2), ("y", 8)]