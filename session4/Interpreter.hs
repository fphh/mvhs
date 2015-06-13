

module Interpreter where

import qualified Data.Map as Map
import Data.Map (Map)

import Control.Applicative (liftA2)

data Expr a =
     Const a
     | Var String
     | Minus (Expr a)
     | Plus (Expr a) (Expr a)
     | Mult (Expr a) (Expr a)
     deriving (Show)

type Env a = Map String a

interpret :: (Num a) => Env a -> Expr a -> Maybe a
interpret _ (Const x) = Just x
interpret ms (Var v) = Map.lookup v ms
interpret ms (Minus x) = negate `fmap` interpret ms x
interpret ms (Plus x y) = liftA2 (+) (interpret ms x) (interpret ms y)
interpret ms (Mult x y) = liftA2 (*) (interpret ms x) (interpret ms y)
