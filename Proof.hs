module Proof where

import Parser
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Hashable
import qualified Data.HashTable.ST.Basic as HT

type Fail = String
type Annotation = String
type CheckedProof = Either Fail [(Logic, Annotation)]
type NumLogic = (Integer, Logic)
type ProofContext s = (HT.HashTable s Logic [NumLogic],
                       HT.HashTable s Logic [NumLogic])

readLogic :: String -> Logic
readLogic = read

pairM :: Monad m => (m a, m b) -> m (a, b)
pairM (f, s) = do
  a <- f
  b <- s
  return (a, b)

axioms :: [NumLogic]
axioms = [ (1, readLogic "a -> b -> a")
         , (2, readLogic "(a -> b) -> (a -> b -> a) -> (a -> c)")
         , (3, readLogic "a -> b -> a & b")
         , (4, readLogic "a & b -> a")
         , (5, readLogic "a & b -> b")
         , (6, readLogic "a -> a | b")
         , (7, readLogic "b -> a | b")
         , (8, readLogic "(a -> c) -> (b -> c) -> (a | b -> c)")
         , (9, readLogic "(a -> b) -> (a -> !b) -> !a")
         , (10, readLogic "!!a -> a")]

compareAsTemplate :: Logic -> Logic -> ST s Bool
compareAsTemplate tmpl expr = do
  table <- HT.new
  let cmpInner (Var s) e = do
        look <- HT.lookup table s
        case look of
          Nothing -> HT.insert table s e >> return True
          Just e' -> return $ e == e'
      cmpInner t e =
          if not (similar t e)
          then return False
          else case t of
                 Not l -> cmpInner l $ firstl e
                 _ -> do
                   f <- cmpInner (firstl t) (firstl e)
                   s <- cmpInner (secondl t) (secondl e)
                   return $ f && s
  cmpInner tmpl expr

appendToHT :: (Hashable k, Eq k) =>  HT.HashTable s k [v] -> k -> v -> ST s ()
appendToHT t k v = do
  res <- HT.lookup t k
  case res of
   Just vs -> HT.insert t k (v:vs)
   Nothing -> HT.insert t k [v]

putInContext :: ProofContext s -> NumLogic -> ST s ()
putInContext (tails, full) nl@(_, expr) = do
  case expr of
    Cons _ end -> appendToHT tails end nl
    _ -> return ()
  appendToHT full expr nl

proofCheck :: [Logic] -> ST s CheckedProof
proofCheck exprs = do
  context <- pairM (HT.new, HT.new)
  counter <- newSTRef 1
  processed <- forM exprs $ \expr -> do
                     res <- checkExpr context expr
                     case res of
                       Just (e, _) -> do
                         i <- readSTRef counter
                         putInContext context (i, e)
                         modifySTRef counter (+ 1)
                         return res
                       Nothing -> return Nothing
  case sequence processed of
   Just list -> return $ Right list
   Nothing -> return $ Left "Высказывание не доказано."

checkExpr :: ProofContext s -> Logic -> ST s (Maybe (Logic, Annotation))
checkExpr context expr = do
  axCheck <- checkForAxiom expr
  case axCheck of
    Just _ -> return axCheck
    Nothing -> checkForMP context expr

checkForAxiom :: Logic -> ST s (Maybe (Logic, Annotation))
checkForAxiom expr = do
  axNum <- newSTRef 0
  forM_ axioms $ \(i, axiom) -> do
    isAxiom <- compareAsTemplate axiom expr
    if isAxiom then writeSTRef axNum i else return ()
  j <- readSTRef axNum
  if j /= 0
     then return $ Just (expr, "сх. акс. " ++ show j)
     else return Nothing

takeList :: (Hashable k, Eq k) => HT.HashTable s k [v] -> k -> ST s [v]
takeList t k = do
  mb <- HT.lookup t k
  return $ maybe [] id mb

checkForMP :: ProofContext s -> Logic -> ST s (Maybe (Logic, Annotation))
checkForMP (tails, fulls) expr = do
  ends <- takeList tails expr
  options <- forM ends $
             \(i, (Cons a _)) -> pairM (return i, takeList fulls a)
  let fetchOption before (_, []) = before
      fetchOption _ (i, ((j, _):_)) = Just (i, j)
      thatsIt = foldl fetchOption Nothing options
  return $ fmap (\(i, j) -> (expr, "M. P. " ++ show j ++ ", " ++ show i)) thatsIt
