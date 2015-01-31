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
type SetMap = [([String], RSet)]

realMax, realMin, inftyPos, inftyNeg :: Double
realMax = 10000000000
realMin = -10000000000
inftyPos = 1/0
inftyNeg = -1/0

findSeparate a b = if b == inftyPos then realMax
                   else if a == inftyNeg then realMin
                        else (a + b) / 2

makeDot :: RSet -> RSet
makeDot ((a, b):ss) = (a, m):(m, b):ss
    where m = findSeparate a b

fraction :: Int -> RSet -> [RSet]
fraction 0 _ = []
fraction 1 rs = [rs]
fraction n ((a, b):ss) = [(a, m)] : fraction (n - 1) ((m, b):ss)
    where m = findSeparate a b

unionSet :: RSet -> RSet -> RSet
unionSet s s' = mrg $ sort $ s ++ s'
    where mrg [] = []
          mrg [x] = [x]
          mrg ((l, r):(l', r'):ss) = if r > l' then mrg $ (l, r'):ss
                                     else (l, r) : (mrg $ (l', r'):ss)

intersectSet :: RSet -> RSet -> RSet
intersectSet s s' = catMaybes [isect a b | a <- s, b <- s']
    where isect (l, r) (l', r') = if nr <= nl then Nothing else Just (nl, nr)
              where nl = max l l'
                    nr = min r r'


allR = [(inftyNeg, inftyPos)]

dfs :: RSet -> KTree -> SetMap
dfs initSet (KTree f _ []) = [(f, initSet)]
dfs initSet (KTree f _ chs) = (f, initSet) : chResults
    where n = length chs
          chResults = if n == 1
                      then dfs (makeDot initSet) $ head chs
                      else concatMap (uncurry dfs) $ zip (fraction n initSet) chs

appendSet :: RSet -> String -> M.Map String RSet -> M.Map String RSet
appendSet s v mp = case M.lookup v mp of
                    Nothing -> M.insert v s mp
                    Just s' -> M.insert v (unionSet s s') mp

setListToMap :: SetMap -> M.Map String RSet
setListToMap = foldr appendAll M.empty
    where appendAll (vars, s) mp = foldr (appendSet s) mp vars

makeUnexample :: Logic -> Maybe (M.Map String RSet)
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
