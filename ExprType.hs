{-# LANGUAGE DeriveDataTypeable #-}
module ExprType where

import Data.Data
import Data.List
import Utils

data Expr = Zero
          | Var String
          | Func String [Expr]
          | Succ Expr
          | Mul Expr Expr
          | Add Expr Expr
            deriving (Eq, Ord, Typeable, Data)

class Expression e where
    isAtom :: e -> Bool
    genEmap :: (e -> Bool) -> (e -> e) -> e -> e
    genEfold :: (e -> Bool) -> (e -> a) -> (a -> a -> a) -> e -> a

    emap :: (e -> e) -> e -> e
    emap = genEmap isAtom

    efold :: (e -> a) -> (a -> a -> a) -> e -> a
    efold = genEfold isAtom

instance Expression Expr where
    isAtom Zero = True
    isAtom (Var _) = True
    isAtom _ = False

    genEmap is f e =
        if is e then f e else
            case e of
              (Func n es) -> Func n $ map (genEmap is f) es
              (Succ a) -> Succ $ genEmap is f a
              (Mul a b) -> Mul (genEmap is f a) (genEmap is f b)
              (Add a b) -> Add (genEmap is f a) (genEmap is f b)
              _ -> f e

    genEfold is f comb e =
        if is e then f e else
            case e of
              g@(Func _ es) -> foldl comb (f g) $ map (genEfold is f comb) es
              (Succ a) -> genEfold is f comb a
              (Mul a b) -> (genEfold is f comb a) `comb` (genEfold is f comb b)
              (Add a b) -> (genEfold is f comb a) `comb` (genEfold is f comb b)
              _ -> f e

instance Show Expr where
    show Zero = "0"
    show (Var s) = s
    show (Func name es) = name ++ "(" ++ intercalate "," (map show es) ++ ")"
    show e@(Succ a) = bracketed (>) a e ++ "'"
    show e@(Mul a b) = bracketed (>) a e ++ "*" ++ bracketed (>) b e
    show e@(Add a b) = bracketed (>) a e ++ "+" ++ bracketed (>) b e
