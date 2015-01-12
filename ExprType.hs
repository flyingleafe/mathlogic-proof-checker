{-# LANGUAGE DeriveDataTypeable #-}
module ExprType where

import Data.Data
import Utils

data Expr = Zero
          | Var String
          | Func String [Expr]
          | Succ Expr
          | Mul Expr Expr
          | Add Expr Expr
            deriving (Eq, Ord, Typeable, Data)

instance Show Expr where
  show Zero = "0"
  show (Var s) = s
  show e@(Succ a) = bracketed (>) a e ++ "'"
  show e@(Mul a b) = bracketed (>) a e ++ "*" ++ bracketed (>) b e
  show e@(Add a b) = bracketed (>) a e ++ "+" ++ bracketed (>) b e
