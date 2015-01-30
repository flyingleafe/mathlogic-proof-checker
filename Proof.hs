{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
module Proof where

import LogicType
import ExprType
import LogicTemplates
import Parser()
import Utils
import Substitutions
import Control.Monad
import Control.Monad.ST
import Control.Applicative
import Data.STRef
import Data.Hashable
import Data.Maybe
import qualified Data.HashTable.ST.Basic as HT

data ErrPlace = Rule | AxScheme deriving Eq
instance Show ErrPlace where
    show Rule = "правило"
    show AxScheme = "схема аксиом"

data ErrType = Fatal
             | NonFreeForSubst Expr Logic String
             | EntersFreely String Logic
             | AssumptionMismatch ErrPlace String Logic
               deriving Eq

instance Show ErrType where
    show Fatal = "Не доказано"
    show (NonFreeForSubst phi a x) =  "терм " ++ show phi ++
                      " не свободен для подстановки в формулу " ++
                      show a ++ " вместо переменной " ++ x
    show (EntersFreely v a) = "переменная " ++ v ++ " входит свободно в формулу " ++ show a
    show (AssumptionMismatch p v e) = "используется " ++ show p ++
                                   " c квантором по переменной " ++ v ++
                                   ", входящей свободно в допущение " ++ show e

data Annotation = Failed ErrType
                | Assumption
                | Axiom Int
                | ArithAxiom Int
                | MP Int Int
                | FA Int
                | EX Int
                  deriving Eq

instance Show Annotation where
  show (Failed s) = show s
  show (Axiom i) = "Сх. акс. " ++ show i
  show (ArithAxiom i) = "Акс. А" ++ show i
  show (MP i j) = "M.P. " ++ show i ++ ", " ++ show j
  show (FA i) = "ForAll " ++ show i
  show (EX i) = "Exists " ++ show i

isFailed (Failed _) = True
isFailed _ = False

type Fail = String
type Proof = [Logic]
type InitContext = [Logic]
type CheckedProof = [(Int, Logic, Annotation)]
type NumLogic = (Int, Logic)
type ProofContext s = (HT.HashTable s Logic [NumLogic],
                       HT.HashTable s Logic NumLogic)

type Error = (Int, ErrType)
data IsEqp = Expr := Expr | NotEqp

veq :: Logic -> IsEqp
veq (Pred (VarID "=" [a, b])) = a := b
veq _ = NotEqp

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
    when (not $ isFailed a) $ do
      putInContext context (i, e)
    modifySTRef counter (+ 1)
    return (i, e, a)

checkExpr :: InitContext -> ProofContext s -> Logic -> ST s (Logic, Annotation)
checkExpr initCtx context expr = do
  if expr `elem` initCtx
    then return (expr, Assumption)
    else (checkForAxiom expr <|> checkForArith expr) `orElse`
         checkForMP context expr >>=
         flip orElse (checkForForall context expr) >>=
         flip orElse (checkForExists context expr) >>=
         (return . fromMaybe (expr, Failed Fatal))

oneAndOnlySubst :: String -> [Subst] -> Maybe Expr
oneAndOnlySubst x l = guard (length l == 1) >> lookup x l

checkAxiomSubst :: String -> Logic -> Logic -> Maybe Expr
checkAxiomSubst x a b =
    if a == b then Just $ Var x
    else oneAndOnlySubst x =<< findSubstL a b

checkAx :: Int -> Logic -> String -> Logic -> Logic -> Maybe (Logic, Annotation)
checkAx n e x a b = do
  phi <- checkAxiomSubst x a b
  if freeForSubst phi x a then return (e, Axiom n)
  else return (e, Failed $ NonFreeForSubst phi a x)

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
checkForAxiom e@(Quant Forall x a :-> b)
    = checkAx 11 e x a b
checkForAxiom e@(a :-> Quant Exists x b)
    = checkAx 12 e x b a
checkForAxiom _ = Nothing

checkForArith :: Logic -> Maybe (Logic, Annotation)
checkForArith e@((veq -> Var "a" := Var "b")
                 :-> (veq -> Succ (Var "a") := Succ (Var "b")))
    = Just (e, ArithAxiom 1)
checkForArith e@((veq -> Var "a" := Var "b") :-> ((veq -> Var "a" := Var "c")
                 :-> (veq -> Var "b" := Var "c")))
    = Just (e, ArithAxiom 2)
checkForArith e@((veq -> Succ (Var "a") := Succ (Var "b"))
                 :-> (veq -> Var "a" := Var "b"))
    = Just (e, ArithAxiom 3)
checkForArith e@(Not (veq -> Succ (Var "a") := Zero))
    = Just (e, ArithAxiom 4)
checkForArith e@(veq -> (Var "a" `Add` Succ (Var "b")) := Succ (Var "a" `Add` Var "b"))
    = Just (e, ArithAxiom 5)
checkForArith e@(veq -> (Var "a" `Add` Zero) := Var "a")
    = Just (e, ArithAxiom 6)
checkForArith e@(veq -> (Var "a" `Mul` Zero) := Zero)
    = Just (e, ArithAxiom 7)
checkForArith e@(veq -> (Var "a" `Mul` Succ (Var "b")) := (Var "a" `Mul` Var "b" `Add` Var "a"))
    = Just (e, ArithAxiom 8)
checkForArith e@(p0 :& Quant Forall x (p :-> p') :-> pf)
    | p == pf &&
      p `hasFreely` x &&
      p0 == substL Zero x p &&
      p' == substL (Succ $ Var x) x p = Just (e, ArithAxiom 9)
checkForArith _ = Nothing

takeList :: (Hashable k, Eq k) => HT.HashTable s k [v] -> k -> ST s [v]
takeList t k = do
  mb <- HT.lookup t k
  return $ fromMaybe [] mb

checkForMP :: ProofContext s -> Logic -> ST s (Maybe (Logic, Annotation))
checkForMP (tails, fulls) expr = do
  ends <- takeList tails expr
  options <- forM ends $
             \(i, (a :-> _)) -> pairM (return i, HT.lookup fulls a)
  let fetchOption before (_, Nothing) = before
      fetchOption _ (i, Just (j, _)) = Just (i, j)
      thatsIt = foldl fetchOption Nothing options
  return $ fmap (\(i, j) -> (expr, MP j i)) thatsIt

checkForForall :: ProofContext s -> Logic -> ST s (Maybe (Logic, Annotation))
checkForForall (_, fulls) expr@(a :-> Quant Forall v b) = do
  prev <- HT.lookup fulls (a :-> b)
  return $ prev >>= \(i, _) ->
    if a `hasFreely` v then Just (expr, Failed $ EntersFreely v a)
    else Just (expr, FA i)
checkForForall _ _ = return Nothing

checkForExists :: ProofContext s -> Logic -> ST s (Maybe (Logic, Annotation))
checkForExists (_, fulls) expr@(Quant Exists v a :-> b) = do
  prev <- HT.lookup fulls (a :-> b)
  return $ prev >>= \(i, _) ->
    if b `hasFreely` v then Just (expr, Failed $ EntersFreely v b)
    else Just (expr, EX i)
checkForExists _ _ = return Nothing
