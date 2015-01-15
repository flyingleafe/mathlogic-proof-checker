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


instance Expression Logic where
    isAtom (Pred _) = True
    isAtom _ = False

    genEmap is f e =
        if is e then f e else
            case e of
              (Quant t n a) -> Quant t n $ genEmap is f a
              (Not a) -> Not $ genEmap is f a
              (a :& b) -> genEmap is f a :& genEmap is f b
              (a :| b) -> genEmap is f a :| genEmap is f b
              (a :-> b) -> genEmap is f a :-> genEmap is f b
              _ -> f e

    genEfold is f _ e | is e = f e
    genEfold is f comb (Quant _ _ e) = genEfold is f comb e
    genEfold is f comb (Not e) = genEfold is f comb e
    genEfold _ f _ p@(Pred _) = f p
    genEfold is f comb e = (genEfold is f comb $ firstl e) `comb` (genEfold is f comb $ secondl e)

instance Show VarID where
    show (VarID s []) = s
    show (VarID s es) = if s == "=" then (head es') ++ "=" ++ (es' !! 1)
                        else s ++ "(" ++ intercalate "," es' ++ ")"
        where es' = map show es

instance Show QType where
    show Forall = "@"
    show Exists = "?"

superComp (Pred (VarID "=" _)) e = True
superComp a e = a > e

instance Show Logic where
    show (Pred s) = show s
    show e@(Quant t s a) = show t ++ s ++ bracketed superComp a e
    show e@(Not a) = '!' : bracketed (>) a e
    show e@(a :& b) = bracketed (>) a e ++ "&" ++ bracketed similarOrGt b e
    show e@(a :| b) = bracketed (>) a e ++ "|" ++ bracketed similarOrGt b e
    show e@(a :-> b) = bracketed similarOrGt a e ++ "->" ++ show b

instance Hashable Logic where
    hashWithSalt salt = hashWithSalt salt . show
