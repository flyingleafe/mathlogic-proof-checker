module Main where

import LogicType
import ExprType
import Parser()
import Utils
import Topology
import System.IO
import Data.List
import qualified Data.Map as M

getVars :: Logic -> [String]
getVars = efold takev union
    where takev (Pred (VarID s _)) = [s]

addEmpties :: [String] -> SetMap -> SetMap
addEmpties l m = foldl ins m l
    where ins mp var = case M.lookup var mp of
                         Nothing -> M.insert var [(0, 0)] mp
                         _ -> mp

showRes :: Logic -> SetMap -> String
showRes l = unlines . map (showSet . snd) . M.toList . addEmpties (getVars l)

main :: IO ()
main = ioFromCmd $ \inp outp -> do
  input <- hGetLine inp
  let expr = read input
      unex = makeUnexample expr
  case unex of
   Nothing -> hPutStrLn outp "Формула общезначима"
   Just mp -> hPutStrLn outp $ showRes expr mp
