{-# LANGUAGE DeriveDataTypeable #-}
module LogicType where

import Data.Hashable
import Data.Typeable
import Data.Data
import Data.List
import Utils
import ExprType

data VarID = VarID String [Expr] deriving (Eq, Ord, Typeable, Data)

data QType = Forall | Exists deriving (Eq, Ord, Typeable, Data)

data Logic = Pred VarID
           | Quant QType String Logic
           | Not Logic
           | Logic :& Logic
           | Logic :| Logic
           | Logic :-> Logic
             deriving (Eq, Ord, Typeable, Data)

isAtom :: Logic -> Bool
isAtom (Pred _) = True
isAtom (Not (Pred _)) = True
isAtom _ = False

firstl :: Logic -> Logic
firstl (Pred _) = error "No subexpr"
firstl (Quant _ _ e) = e
firstl (Not e) = e
firstl (e :| _) = e
firstl (e :& _) = e
firstl (e :-> _) = e

secondl :: Logic -> Logic
secondl (Pred _) = error "No subexpr"
secondl (Not _) = error "No subexpr"
secondl (Quant _ _ _) = error "No subexpr"
secondl (_ :| e) = e
secondl (_ :& e) = e
secondl (_ :-> e) = e

similar :: Logic -> Logic -> Bool
similar (Pred _) (Pred _) = True
similar (Quant _ _ _) (Quant _ _ _) = True
similar (Not _) (Not _) = True
similar (_ :& _) (_ :& _) = True
similar (_ :| _) (_ :| _) = True
similar (_ :-> _) (_ :-> _) = True
similar _ _ = False

similarOrGt :: Logic -> Logic -> Bool
similarOrGt a b = similar a b || (a > b)

instance Show VarID where
  show (VarID s []) = s
  show (VarID s es) = if s == "=" then (head es') ++ "=" ++ (es' !! 1)
              else s ++ "(" ++ intercalate "," es' ++ ")"
                  where es' = map show es

instance Show QType where
  show Forall = "@"
  show Exists = "?"

instance Show Logic where
  show (Pred s) = show s
  show e@(Quant t s a) = show t ++ s ++ bracketed similarOrGt a e
  show e@(Not a) = '!' : bracketed (>) a e
  show e@(a :& b) = bracketed (>) a e ++ "&" ++ bracketed similarOrGt b e
  show e@(a :| b) = bracketed (>) a e ++ "|" ++ bracketed similarOrGt b e
  show e@(a :-> b) = bracketed similarOrGt a e ++ "->" ++ show b

instance Hashable Logic where
  hashWithSalt salt = hashWithSalt salt . show
