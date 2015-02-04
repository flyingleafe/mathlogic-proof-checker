{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Topology where

import Utils
import LogicType
import Kripke
import Data.List
import Data.Maybe
import Numeric
import qualified Data.Map as M

data Edge a = Open a | Closed a deriving (Eq, Show)

unwrap :: Edge a -> a
unwrap (Open a) = a
unwrap (Closed a) = a

instance Ord a => Ord (Edge a) where
    compare a b = compare (unwrap a) (unwrap b)

instance Functor Edge where
    fmap f (Open a) = Open $ f a
    fmap f (Closed a) = Closed $ f a

instance Num a => Num (Edge a) where
    (Closed a) + (Closed b) = Closed $ a + b
    a + b = Open $ unwrap a + unwrap b
    (Closed a) * (Closed b) = Closed $ a * b
    abs a = fmap abs a
    fromInteger i = Open $ fromInteger i
    negate a = fmap negate a
    signum a = fmap signum a

type RGap = (Edge Double, Edge Double)
type RSet = [RGap]
type SetList = [([String], RSet)]
type SetMap = M.Map String RSet

realMax, realMin, inftyPos, inftyNeg :: Double
realMax = 10000000000
realMin = -10000000000
inftyPos = 1/0
inftyNeg = -1/0

infixl 6 ~=
(~=) :: Eq a => Edge a -> Edge a -> Bool
a ~= b = unwrap a == unwrap b

findSeparate a b
  | unwrap a == inftyNeg && unwrap b == inftyPos = 0
  | unwrap a == inftyNeg = min realMin (unwrap b - 10)
  | unwrap b == inftyPos = max realMax (unwrap a + 10)
  | otherwise = (unwrap a + unwrap b) / 2

makeDot :: RSet -> RSet
makeDot ((a, b):ss) = (a, m):(m, b):ss
    where m = Open $ findSeparate a b

fraction :: Int -> RSet -> [RSet]
fraction 0 _ = []
fraction 1 rs = [rs]
fraction n ((a, b):ss) = [(a, m)] : fraction (n - 1) ((m, b):ss)
    where m = Open $ findSeparate a b

intern :: RSet -> RSet
intern = filter $ \(a, b) -> a < b

edgesFit :: (Edge Double -> Edge Double -> Bool) -> Edge Double -> Edge Double -> Bool
edgesFit f r l
    | r > l = True
    | r < l = False
    | otherwise = f r l

matches = edgesFit $ mtch
    where mtch (Open _) (Open _) = False
          mtch _ _ = True

overlaps = edgesFit $ ovrl
    where ovrl (Closed _) (Closed _) = True
          ovrl _ _ = False

turn :: Edge Double -> Edge Double
turn (Open a) = Closed a
turn (Closed a) = Open a

unionSet ::  RSet -> RSet -> RSet
unionSet s s' = mrg $ sort $ s ++ s'
    where mrg [] = []
          mrg [x] = [x]
          mrg ((l, r):(l', r'):ss) = if r `matches` l' then mrg $ (l, r'):ss
                                     else (l, r) : (mrg $ (l', r'):ss)

intersectSet :: RSet -> RSet -> RSet
intersectSet s s' = catMaybes [isect a b | a <- s, b <- s']
    where isect (l, r) (l', r') = if nr `overlaps` nl then Nothing else Just (nl, nr)
              where nl = max l l'
                    nr = min r r'

negateSet' :: RSet -> RSet
negateSet' [] = [(Open inftyNeg, Open inftyPos)]
negateSet' [(a, b)] = [(Open inftyNeg, turn a), (turn b, Open inftyPos)]
negateSet' ((a, b):ss) = [(Open inftyNeg, turn a), (turn b, Open inftyPos)]
                        `intersectSet` negateSet' ss

negateSet = intern . negateSet'

implySet :: RSet -> RSet -> RSet
implySet s s' = intern $ negateSet' s `unionSet` s'

allR = [(Open inftyNeg, Open inftyPos)]

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

showBorder :: Edge Double -> String
showBorder k
    | unwrap k == inftyNeg = "-inf"
    | unwrap k == inftyPos = "+inf"
    | otherwise = showFFloat Nothing (unwrap k) ""

showGap :: RGap -> String
showGap (a, b) = "(" ++ showBorder a ++ "," ++ showBorder b ++ ")"

showSet :: RSet -> String
showSet = concatMap showGap

topEval :: Logic -> SetMap -> RSet
topEval (Pred (VarID s _)) mp = fromMaybe [] $ M.lookup s mp
topEval (Not l) mp = negateSet $ topEval l mp
topEval (a :| b) mp = topEval a mp `unionSet` topEval b mp
topEval (a :& b) mp = topEval a mp `intersectSet` topEval b mp
topEval (a :-> b) mp = topEval a mp `implySet` topEval b mp
