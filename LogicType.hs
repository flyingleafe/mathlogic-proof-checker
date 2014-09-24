module LogicType where

import Data.Hashable

data Logic = Var String
           | Logic `And` Logic
           | Logic `Or` Logic
           | Not Logic
           | Logic `Cons` Logic
             deriving (Eq, Ord)

firstl :: Logic -> Logic
firstl (Var _) = error "No subexpr"
firstl (Not e) = e
firstl (Or e _) = e
firstl (And e _) = e
firstl (Cons e _) = e

secondl :: Logic -> Logic
secondl (Var _) = error "No subexpr"
secondl (Not _) = error "No subexpr"
secondl (Or _ e) = e
secondl (And _ e) = e
secondl (Cons _ e) = e

similar :: Logic -> Logic -> Bool
similar (Var _) (Var _) = True
similar (Not _) (Not _) = True
similar (And _ _) (And _ _) = True
similar (Or _ _) (Or _ _) = True
similar (Cons _ _) (Cons _ _) = True
similar _ _ = False

similarOrGt :: Logic -> Logic -> Bool
similarOrGt a b = similar a b || (a > b)

bracketed :: Show a => (a -> t -> Bool) -> a -> t -> [Char]
bracketed comp a e
    | a `comp` e = "(" ++ show a ++ ")"
    | otherwise = show a

instance Show Logic where
  show (Var s) = s
  show e@(Not a) = '!' : bracketed (>) a e
  show e@(a `And` b) = bracketed (>) a e ++ "&" ++ bracketed similarOrGt b e
  show e@(a `Or` b) = bracketed (>) a e ++ "|" ++ bracketed similarOrGt b e
  show e@(a `Cons` b) = bracketed similarOrGt a e ++ "->" ++ show b

instance Hashable Logic where
  hashWithSalt salt = hashWithSalt salt . show
