

module Differentiation where

import qualified Data.Map as Map
import Interpreter (Expr(..), interpret, (.+), (.-), (.*), pretty, Env)

x :: Expr a
x = Var "x"

_3 :: Expr Double
_3 = Const 3

_7 :: Expr Double
_7 = Const 7

p :: Expr Double
p = _7 .- (x .* x .* x)

diff :: (Num a) => Expr a -> Expr a
diff (Const _) = Const 0
diff (Var str) = Const 1
diff (Minus a) = Minus (diff a)
diff (Plus a b) = Plus (diff a) (diff b)
diff (Mult a b) = Plus (Mult a (diff b)) (Mult (diff a) b)


next :: Fractional a => (a -> a) -> (a -> a) -> a -> a
next f f' xn = xn - ((f xn) / (f' xn))

newton :: Fractional a => Expr a -> a -> [a]
newton f =
  let env x = Map.singleton "x" x
      f' = diff f
      g' = flip interpret f' . env
      g = flip interpret f . env
  in iterate (next g g') 
