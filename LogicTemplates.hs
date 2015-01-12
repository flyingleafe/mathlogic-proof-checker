{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module LogicTemplates where

import LogicType
import Parser
import Utils
import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Data.ByteString.Char8 (pack)

type FilePos = (String, Int, Int)
type VarTrans a = Logic -> Maybe (TH.Q a)

quoteLogicProof :: String -> TH.Q TH.Exp
quoteLogicProof s = do
  pos <- getPosition
  e <- parseLogicProof pos (pack s)
  dataToExpQ (const Nothing `extQ` takeExpVar) e

quoteLogicPat :: String -> TH.Q TH.Pat
quoteLogicPat s = do
  pos <- getPosition
  e <- parseLogicExp pos (pack s)
  dataToPatQ (const Nothing `extQ` takePatternVar) e

takeVar :: (TH.Name -> TH.Q a) -> VarTrans a
takeVar namef p@(Pred _) =  Just $ namef (TH.mkName $ lowercase $ show p)
takeVar _ _ = Nothing

takePatternVar = takeVar TH.varP
takeExpVar = takeVar TH.varE

getPosition :: TH.Q FilePos
getPosition = fmap transPos TH.location where
  transPos loc = (TH.loc_filename loc,
                  fst (TH.loc_start loc),
                  snd (TH.loc_start loc))

parseLogicProof = parseT proof
parseLogicExp = parseT expr

logic :: QuasiQuoter
logic = QuasiQuoter
  { quoteExp = quoteLogicProof
  , quotePat = quoteLogicPat
  , quoteDec = undefined
  , quoteType = undefined
  }

logicF = quoteFile logic
