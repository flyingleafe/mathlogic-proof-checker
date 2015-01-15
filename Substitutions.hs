module Substitutions where

import LogicType
import ExprType
import Utils
import Data.List
import Control.Monad

type Subst = (String, Expr)

substE :: Expr -> String -> Expr -> Expr
substE b x a = emap go a
    where go v@(Var s) = if x == s then b else v
          go Zero = Zero

substL :: Expr -> String -> Logic -> Logic
substL b x a = genEmap stopQ go a
    where go (Pred (VarID n es)) =  Pred $ VarID n $ map (substE b x) es
          go q@(Quant t v e) = if x == v then q else Quant t v $ substL b x e
          stopQ (Quant _ _ _) = True
          stopQ (Pred _) = True
          stopQ _ = False

getVars :: Expr -> [String]
getVars = efold gvars union
    where gvars (Var s) = [s]
          gvars _ = []

hasVar :: String -> Expr -> Bool
hasVar v = efold (hvar v) (||)
    where hvar x (Var s) = x == s
          hvar _ _ = False

getFreeVars :: Logic -> [String]
getFreeVars = gfree []
    where gfree bnd (Quant _ n e) = gfree (n:bnd) e
          gfree bnd (Pred (VarID _ es)) = unionMap getVars es \\ bnd
          gfree bnd (Not e) = gfree bnd e
          gfree bnd e = gfree bnd (firstl e) `union` gfree bnd (secondl e)

hasFreely e x = x `elem` getFreeVars e

getBoundedNear :: String -> Logic -> [String]
getBoundedNear = gbn []
    where gbn bnd v (Quant _ n e) = if v == n then bnd else gbn (n:bnd) v e
          gbn bnd v (Pred (VarID _ es)) = if any (hasVar v) es then bnd else []
          gbn bnd v (Not e) = gbn bnd v e
          gbn bnd v e = gbn bnd v (firstl e) `union` gbn bnd v (secondl e)

freeForSubst :: Expr -> String -> Logic -> Bool
freeForSubst e x phi = (getBoundedNear x phi `intersect` getVars e) == []

diff :: Expr -> Expr -> Maybe [Subst]
diff (Var s) e = Just [(s, e)]
diff Zero Zero = Just []
diff Zero _ = Nothing
diff (Func f es) (Func f' es')
    | f == f' && length es == length es' =
         fmap unite $ sequence $ map (uncurry diff) $ zip es es'
    | otherwise = Nothing
diff (Succ e) (Succ e') = diff e e'
diff (Add x y) (Add x' y') = liftM2 union (diff x x') (diff y y')
diff (Mul x y) (Mul x' y') = liftM2 union (diff x x') (diff y y')
diff _ _  = Nothing


checkClashing :: [String] -> [Subst] -> Maybe [Subst]
checkClashing found ((s, e):ss) =
    if s `elem` found then Nothing
    else fmap ((s, e):) $ checkClashing (s:found) ss
checkClashing _ [] = Just []

findSubstE :: Expr -> Expr -> Maybe [Subst]
findSubstE a b = fmap (filter $ not . same) $ diff a b >>= checkClashing []
    where same (s, Var s') | s == s' = True
          same _ = False

almostEquals :: Logic -> Logic -> Bool
almostEquals (Pred (VarID s es)) (Pred (VarID s' es')) = s == s' && length es == length es'
almostEquals (Quant t v e) (Quant t' v' e') = t == t' && v == v' && almostEquals e e'
almostEquals (Not e) (Not e') = almostEquals e e'
almostEquals a b | similar a b = almostEquals (firstl a) (firstl b) && almostEquals (secondl a) (secondl b)
almostEquals _ _ = False

findSubstL :: Logic -> Logic -> Maybe [Subst]
findSubstL a b
    | not $ almostEquals a b = Nothing
    | otherwise = fs [] a b
    where fs bnd (Pred (VarID _ es)) (Pred (VarID _ es'))
              = sequence (zipWith findSubstE es es') >>= (checkClashing bnd . unite)
          fs bnd (Quant _ n e) (Quant _ _ e') = fs (n:bnd) e e'
          fs bnd (Not e) (Not e') = fs bnd e e'
          fs bnd x y = liftM2 union (fs bnd (firstl x) (firstl y)) (fs bnd (secondl x) (secondl y))
