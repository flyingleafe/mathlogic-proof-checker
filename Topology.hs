{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Topology where

import Utils
import LogicType
import Kripke
import Data.List
import Data.Maybe
import Numeric
import qualified Data.Map as M

type RGap = (Double, Double)
type RSet = [RGap]
type SetList = [([String], RSet)]
type SetMap = M.Map String RSet

realMax, realMin, inftyPos, inftyNeg :: Double
realMax = 10000000000
realMin = -10000000000
inftyPos = 1/0
inftyNeg = -1/0

findSeparate a b = if b == inftyPos then realMax
                   else if a == inftyNeg then b - 10
                        else (a + b) / 2

makeDot :: RSet -> RSet
makeDot ((a, b):ss) = (a, m):(m, b):ss
    where m = findSeparate a b

fraction :: Int -> RSet -> [RSet]
fraction 0 _ = []
fraction 1 rs = [rs]
fraction n ((a, b):ss) = [(a, m)] : fraction (n - 1) ((m, b):ss)
    where m = findSeparate a b

intern :: RSet -> RSet
intern = filter $ \(a, b) -> a < b

unionSetGen :: (Double -> Double -> Bool) -> RSet -> RSet -> RSet
unionSetGen compOp s s' = intern $ mrg $ sort $ s ++ s'
    where mrg [] = []
          mrg [x] = [x]
          mrg ((l, r):(l', r'):ss) = if r `compOp` l' then mrg $ (l, r'):ss
                                     else (l, r) : (mrg $ (l', r'):ss)

unionSet = unionSetGen (>)
unionSet' = unionSetGen (>=)

intersectSet :: RSet -> RSet -> RSet
intersectSet s s' = catMaybes [isect a b | a <- s, b <- s']
    where isect (l, r) (l', r') = if nr < nl then Nothing else Just (nl, nr)
              where nl = max l l'
                    nr = min r r'

negateSet' :: RSet -> RSet
negateSet' [] = [(inftyNeg, inftyPos)]
negateSet' [(a, b)] = [(inftyNeg, a), (b, inftyPos)]
negateSet' ((a, b):ss) = [(inftyNeg, a), (b, inftyPos)]
                        `intersectSet` negateSet' ss

negateSet = intern . negateSet'

implySet :: RSet -> RSet -> RSet
implySet s s' = intern $ negateSet' s `unionSet'` s'

allR = [(inftyNeg, inftyPos)]

dfs :: RSet -> KTree -> SetList
dfs initSet (KTree f _ []) = [(f, initSet)]
dfs initSet (KTree f _ chs) = (f, initSet) : chResults
    where n = length chs
          chResults = if n == 1
                      then dfs (makeDot initSet) $ head chs
                      else concatMap (uncurry dfs) $ zip (fraction n initSet) chs

appendSet :: RSet -> String -> SetMap -> SetMap
appendSet s v mp = case M.lookup v mp of
                    Nothing -> M.insert v s mp
                    Just s' -> M.insert v (unionSet s s') mp

setListToMap :: SetList -> SetMap
setListToMap = foldr appendAll M.empty
    where appendAll (vars, s) mp = foldr (appendSet s) mp vars

makeUnexample :: Logic -> Maybe SetMap
makeUnexample = fmap (setListToMap . dfs allR) . unProve

showBorder :: Double -> String
showBorder k
    | k == inftyNeg = "-inf"
    | k == inftyPos = "+inf"
    | otherwise = showFFloat Nothing k ""

showGap :: RGap -> String
showGap (a, b) = "(" ++ showBorder a ++ "," ++ showBorder b ++ ")"

showSet :: RSet -> String
showSet = concatMap showGap

topEval :: Logic -> SetMap -> RSet
topEval (Pred (VarID s _)) mp = fromMaybe allR $ M.lookup s mp
topEval (Not l) mp = negateSet $ topEval l mp
topEval (a :| b) mp = topEval a mp `unionSet` topEval b mp
topEval (a :& b) mp = topEval a mp `intersectSet` topEval b mp
topEval (a :-> b) mp = topEval a mp `implySet` topEval b mp
