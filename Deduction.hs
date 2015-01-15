{-# LANGUAGE QuasiQuotes #-}
module Deduction where

import LogicType
import LogicTemplates
import Parser()
import Proof
import Substitutions
import Control.Monad
import Control.Monad.ST
import Data.Maybe

type Heading = (InitContext, Logic)
data DeductionContext = DC { movingExpr :: Logic
                           , list :: [(Int, (Logic, Annotation))]
                             }

isValidProof :: CheckedProof -> Either Error CheckedProof
isValidProof = sequence . map go
    where go (i, _, Failed err) = Left (i, err)
          go e = Right e

deducable :: Logic -> CheckedProof -> Either Error CheckedProof
deducable me = sequence . map go
    where go e@(_, a :-> b, Axiom 11) = checkQ e a AxScheme
          go e@(_, a :-> b, Axiom 12) = checkQ e b AxScheme
          go e@(_, a :-> b, FA _) = checkQ e b Rule
          go e@(_, a :-> b, EX _) = checkQ e a Rule
          go e = Right e
          checkQ e@(i, _, _) (Quant _ v _) place
              | me `hasFreely` v = Left (i, AssumptionMismatch place v me)
              | otherwise = Right e


applyDeduction :: Heading -> Proof -> Either Error (Heading, Proof)
applyDeduction h@([], _) proof = Right (h, proof)
applyDeduction (ctx, expr) proof = do
  let newCtx = init ctx
      me = last ctx
  checked <- deducable me =<< (isValidProof $ runST $ proofCheck ctx proof)
  let checked' = map (\(i, e, a) -> (i, (e, a))) checked
      context = DC me checked'
  return ((newCtx, me :-> expr), rebuildProof context)

rebuildProof :: DeductionContext -> Proof
rebuildProof (DC me ls) = concatMap rebuild ls
    where rebuild (_, (e, a)) =
            if e == me then selfImpl e
            else case a of
                   MP i j -> rebuildMP me e (get i) (get j)
                   FA i -> rebuildFA me e (get i)
                   EX i -> rebuildEX me e (get i)
                   _ -> rebuildKnown me e
          get i = fst $ fromJust $ lookup i ls

selfImpl :: Logic -> Proof
selfImpl a = [logicF|TextProofs/SelfCons.txt|]

rebuildMP :: Logic -> Logic -> Logic -> Logic -> Proof
rebuildMP me e bi bj =
  [ (me :-> bi) :->
    ((me :-> bj) :-> (me :-> e))
  , (me :-> bj) :-> (me :-> e)
  , (me :-> e)]

rebuildFA :: Logic -> Logic -> Logic -> Proof
rebuildFA a (b' :-> c') (b :-> c) = prependAnd a b c ++ removeAnd a b c'

rebuildEX :: Logic -> Logic -> Logic -> Proof
rebuildEX a (b' :-> c') (b :-> c) = rotateImpl a b c ++ rotateImpl b' a c

rebuildKnown :: Logic -> Logic -> Proof
rebuildKnown me e =
  [ e
  , e :-> (me :-> e)
  , me :-> e]

prependAnd, removeAnd, rotateImpl :: Logic -> Logic -> Logic -> Proof
prependAnd a b c = [logicF|TextProofs/PrependAnd.txt|]
removeAnd a b c = [logicF|TextProofs/RemoveAnd.txt|]
rotateImpl a b c = [logicF|TextProofs/RotateImpl.txt|]
