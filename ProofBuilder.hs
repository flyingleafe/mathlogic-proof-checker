{-# LANGUAGE QuasiQuotes #-}
module ProofBuilder where

import LogicType
import Proof
import Deduction
import Utils
import Data.List
import Data.Maybe
import Data.Tuple
import Control.Monad
import Control.Applicative
import LogicTemplates

takeVarList :: Logic -> [VarID]
takeVarList (Pred s) = [s]
takeVarList (Not e) = takeVarList e
takeVarList e = nub $ takeVarList (firstl e) ++ takeVarList (secondl e)

genAllValues :: [Logic] -> [InitContext]
genAllValues [] = []
genAllValues [v] = [[v], [Not v]]
genAllValues (v:vs) = map (v :) rest ++ map (Not v :) rest
    where rest = genAllValues vs

makeEstims :: Logic -> [InitContext]
makeEstims = genAllValues . map Pred . takeVarList

estimDirect :: Logic -> InitContext -> Maybe Bool
estimDirect e est = if e `elem` est then Just True
                    else if (Not e) `elem` est then Just False
                         else Nothing

estimate' :: Logic -> InitContext -> Maybe Bool
estimate' e@(Pred _) est = estimDirect e est
estimate' (Not e) est = fmap not $ estimate e est
estimate' (a :| b) est = (||) <$> estimate a est <*> estimate b est
estimate' (a :& b) est = (&&) <$> estimate a est <*> estimate b est
estimate' (a :-> b) est = (||) <$> (fmap not $ estimate a est) <*> estimate b est

estimate e est = estimDirect e est <|> estimate' e est
estimate'' = ((.).(.)) fromJust estimate

findBadEstim :: Logic -> [InitContext] -> InitContext
findBadEstim e ests = case filter (not . estimate'' e) ests of
  [] -> []
  (c:cs) -> c

estimDesc :: InitContext -> [(String, String)]
estimDesc est = map go est
  where go (Not e) = (show e, "Л")
        go e = (show e, "И")

cutProof :: Logic -> Proof -> Proof
cutProof e p = take ((+1) $ fromJust $ elemIndex e p) $ fastNub p

buildProof :: Logic -> Either [(String, String)] Proof
buildProof e = maybeToEither result $ estimDesc $ findBadEstim e estims
  where result = fmap (cutProof e) $ mergeProofs contextedProofs
        contextedProofs = zip estims proofs
        estims = makeEstims e
        proofs = mapMaybe (buildProofEstimated e) estims


mergeProofs :: [(InitContext, Proof)] -> Maybe Proof
mergeProofs [([], p)] = Just p
mergeProofs epp = pairs epp >>= mergeProofs . map merge2
    where merge2 ((c1, p1), (c2, p2)) = (newc, np1 ++ np2 ++ tnd ++ excl)
            where ((newc, r :-> a), np1) = fromEither $ applyDeduction (c1, last p1) p1
                  (_, np2) = fromEither $ applyDeduction (c2, last p2) p2
                  tnd = tertiumNonDatum r
                  excl = [logicF|TextProofs/Exclusion.txt|]

buildProofEstimated :: Logic -> InitContext -> Maybe Proof
buildProofEstimated expr est =
  if not $ estimate'' expr est then Nothing else
      case expr of
        (Pred _) -> Just [expr]
        (Not (Pred _)) -> Just [expr]
        (Not (Not a)) -> concatSeq [buildProofEstimated a est, makeLemmaSingle a expr]
        _ -> concatSeq [firstProof, secondProof, makeLemma a b expr]
            where (a, b) = head $ filter (goodPair est) $ subPairs expr
                  firstProof = buildProofEstimated a est
                  secondProof = buildProofEstimated b est

takeIn :: Logic -> Logic
takeIn (Not a) = a
takeIn a = a

subPairs :: Logic -> [(Logic, Logic)]
subPairs e = filterPairs $ allContexts $ takeIn e
    where toPair (x:y:_) = (x, y)
          allContexts x = genAllValues [firstl x, secondl x]
          filterPairs = map toPair . filter (estimate'' e)

goodPair :: InitContext -> (Logic, Logic) -> Bool
goodPair est = uncurry (&&) . mapPair (flip estimate'' est)

makeLemmaSingle :: Logic -> Logic -> Maybe Proof
makeLemmaSingle = makeLemma (Pred $ VarID "X" [])

makeLemma :: Logic -> Logic -> Logic -> Maybe Proof
makeLemma a b ab = if fmap last lemma == Just ab then lemma else Nothing
    where lemma = takeFile a b ab

takeFile :: Logic -> Logic -> Logic -> Maybe Proof
takeFile a' b' (Not (a :| b))
  | a' == Not a && b' == Not b = Just [logicF|TextProofs/NoNoNotOr.txt|]
takeFile a' b' (Not (a :& b))
  | a' == Not a && b' == Not b = Just [logicF|TextProofs/NoNoNotAnd.txt|]
  | a' == Not a = Just [logicF|TextProofs/NoYesNotAnd.txt|]
  | b' == Not b = Just [logicF|TextProofs/YesNoNotAnd.txt|]
takeFile a' b' (Not (a :-> b))
  | a' == a && b' == Not b = Just [logicF|TextProofs/YesNoNotCons.txt|]
takeFile a' b' (a :| b)
  | a' == a && b' == b = Just [logicF|TextProofs/YesYesOr.txt|]
  | a' == a = Just [logicF|TextProofs/YesNoOr.txt|]
  | b' == b = Just [logicF|TextProofs/NoYesOr.txt|]
takeFile a' b' (a :& b)
  | a' == a && b' == b = Just [logicF|TextProofs/YesYesAnd.txt|]
takeFile a' b' (a :-> b)
  | a' == a && b' == b = Just [logicF|TextProofs/YesYesCons.txt|]
  | b == b' = Just [logicF|TextProofs/NoYesCons.txt|]
  | a' == Not a = Just [logicF|TextProofs/NoNoCons.txt|]
takeFile _ a (Not (Not a')) | a == a' = Just [logicF|TextProofs/AddDoubleNot.txt|]
takeFile _ _ _ = Nothing

contraposition :: Logic -> Logic -> Proof
contraposition a b = [logicF|TextProofs/Contraposition.txt|]

tertiumNonDatum :: Logic -> Proof
tertiumNonDatum a = [a :-> (a :| Not a), Not a :-> (a :| Not a)]
                    ++ contraposition a (a :| Not a)
                    ++ contraposition (Not a) (a :| Not a)
                    ++ [Not (a :| Not a) :-> Not a, Not (a :| Not a) :-> Not (Not a)]
                    ++ [logicF|TextProofs/TertiumNonDatum.txt|]
