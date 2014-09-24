module Proof where

import LogicType
import Parser()
import Utils
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Hashable
import qualified Data.HashTable.ST.Basic as HT

data Annotation = Failed
                | Assumption
                | Axiom Int
                | MP Int Int
                  deriving Eq

instance Show Annotation where
  show Failed = "Не доказано"
  show (Axiom i) = "Сх. акс. " ++ show i
  show (MP i j) = "M.P. " ++ show i ++ ", " ++ show j

type Fail = String
type Proof = [Logic]
type InitContext = [Logic]
type CheckedProof = [(Int, Logic, Annotation)]
type NumLogic = (Int, Logic)
type ProofContext s = (HT.HashTable s Logic [NumLogic],
                       HT.HashTable s Logic NumLogic)

readLogic :: String -> Logic
readLogic = read

axioms :: [NumLogic]
axioms = [ (1, readLogic "a -> b -> a")
         , (2, readLogic "(a -> b) -> (a -> b -> c) -> (a -> c)")
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

appendToHT :: (Hashable k, Eq k) => HT.HashTable s k [v] -> k -> v -> ST s ()
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
  HT.insert full expr nl

proofCheck :: InitContext -> Proof -> ST s CheckedProof
proofCheck initCtx exprs = do
  context <- pairM (HT.new, HT.new)
  counter <- newSTRef 1
  forM exprs $ \expr -> do
    (e, a) <- checkExpr initCtx context expr
    i <- readSTRef counter
    when (a /= Failed) $ do
      putInContext context (i, e)
    modifySTRef counter (+ 1)
    return (i, e, a)

checkExpr :: InitContext -> ProofContext s -> Logic -> ST s (Logic, Annotation)
checkExpr initCtx context expr = do
  if expr `elem` initCtx
    then return (expr, Assumption)
    else do
      axCheck <- checkForAxiom expr
      case axCheck of
       Just e -> return e
       Nothing -> do
         mpres <- checkForMP context expr
         return $ maybe (expr, Failed) id mpres

checkForAxiom :: Logic -> ST s (Maybe (Logic, Annotation))
checkForAxiom expr = do
  axNum <- newSTRef 0
  forM_ axioms $ \(i, axiom) -> do
    isAxiom <- compareAsTemplate axiom expr
    if isAxiom then writeSTRef axNum i else return ()
  j <- readSTRef axNum
  if j /= 0
     then return $ Just (expr, Axiom j)
     else return Nothing

takeList :: (Hashable k, Eq k) => HT.HashTable s k [v] -> k -> ST s [v]
takeList t k = do
  mb <- HT.lookup t k
  return $ maybe [] id mb

checkForMP :: ProofContext s -> Logic -> ST s (Maybe (Logic, Annotation))
checkForMP (tails, fulls) expr = do
  ends <- takeList tails expr
  options <- forM ends $
             \(i, (Cons a _)) -> pairM (return i, HT.lookup fulls a)
  let fetchOption before (_, Nothing) = before
      fetchOption _ (i, Just (j, _)) = Just (i, j)
      thatsIt = foldl fetchOption Nothing options
  return $ fmap (\(i, j) -> (expr, MP j i)) thatsIt
