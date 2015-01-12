{-# LANGUAGE QuasiQuotes #-}
module Deduction where

import LogicType
import LogicTemplates
import Parser()
import Proof
import Control.Monad.ST
import Data.Maybe

type Heading = (InitContext, Logic)
data DeductionContext = DC { movingExpr :: Logic
                           , list :: [(Int, (Logic, Annotation))]
                             }

applyDeduction :: Heading -> Proof -> (Heading, Proof)
applyDeduction (ctx, expr) proof =
  let newCtx = init ctx
      me = last ctx
      checked = runST $ proofCheck ctx proof
      checked' = map (\(i, e, a) -> (i, (e, a))) checked
      context = DC (last ctx) checked'
  in
  ((newCtx, me :-> expr), rebuildProof context)

rebuildProof :: DeductionContext -> Proof
rebuildProof (DC me ls) = concatMap rebuild ls
    where rebuild (_, (e, a)) =
            if e == me then selfImpl e
            else case a of
                   MP i j -> rebuildMP me e (get i) (get j)
                   _ -> rebuildKnown me e
          get i = fst $ fromMaybe undefined $ lookup i ls

selfImpl :: Logic -> Proof
selfImpl a = [logicF|TextProofs/SelfCons.txt|]

rebuildMP :: Logic -> Logic -> Logic -> Logic -> Proof
rebuildMP me e bi bj =
  [ (me :-> bi) :->
    ((me :-> bj) :-> (me :-> e))
  , (me :-> bj) :-> (me :-> e)
  , (me :-> e)]

rebuildKnown :: Logic -> Logic -> Proof
rebuildKnown me e =
  [ e
  , e :-> (me :-> e)
  , me :-> e]
