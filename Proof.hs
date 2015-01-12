{-# LANGUAGE QuasiQuotes #-}
module Proof where

import LogicType
import LogicTemplates
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

appendToHT :: (Hashable k, Eq k) => HT.HashTable s k [v] -> k -> v -> ST s ()
appendToHT t k v = do
  res <- HT.lookup t k
  case res of
   Just vs -> HT.insert t k (v:vs)
   Nothing -> HT.insert t k [v]

putInContext :: ProofContext s -> NumLogic -> ST s ()
putInContext (tails, full) nl@(_, expr) = do
  case expr of
   _ :-> end -> appendToHT tails end nl
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
      let axCheck = checkForAxiom expr
      case axCheck of
       Just e -> return e
       Nothing -> do
         mpres <- checkForMP context expr
         return $ maybe (expr, Failed) id mpres

checkForAxiom :: Logic -> Maybe (Logic, Annotation)
checkForAxiom e@[logic|A -> B -> C|]
    | a == c = Just (e, Axiom 1)
checkForAxiom e@[logic|(A -> B) -> (C -> D -> F) -> (G -> H)|]
    | a == c && a == g && f == h && b == d = Just (e, Axiom 2)
checkForAxiom e@[logic|A -> B -> C & D|]
    | a == c && b == d = Just (e, Axiom 3)
checkForAxiom e@[logic|A & B -> C|]
    | a == c = Just (e, Axiom 4)
    | b == c = Just (e, Axiom 5)
checkForAxiom e@[logic|A -> B | C|]
    | a == b = Just (e, Axiom 6)
    | a == c = Just (e, Axiom 7)
checkForAxiom e@[logic|(A -> C) -> (B -> D) -> (F | G -> H)|]
    | c == d && a == f && b == g && c == h = Just (e, Axiom 8)
checkForAxiom e@[logic|(A -> B) -> (C -> !D) -> !F|]
    | a == c && b == d && a == f = Just (e, Axiom 9)
checkForAxiom e@[logic|!!A -> B|]
    | a == b  = Just (e, Axiom 10)
checkForAxiom _ = Nothing

takeList :: (Hashable k, Eq k) => HT.HashTable s k [v] -> k -> ST s [v]
takeList t k = do
  mb <- HT.lookup t k
  return $ maybe [] id mb

checkForMP :: ProofContext s -> Logic -> ST s (Maybe (Logic, Annotation))
checkForMP (tails, fulls) expr = do
  ends <- takeList tails expr
  options <- forM ends $
             \(i, (a :-> _)) -> pairM (return i, HT.lookup fulls a)
  let fetchOption before (_, Nothing) = before
      fetchOption _ (i, Just (j, _)) = Just (i, j)
      thatsIt = foldl fetchOption Nothing options
  return $ fmap (\(i, j) -> (expr, MP j i)) thatsIt
