module Main where

import Proof
import Parser
import System.IO
import System.Environment
import Control.Monad.ST

mSeq :: Monad m => (a -> m b) -> [a] -> m b
mSeq _ [] = error "Empty list"
mSeq f [x] = f x
mSeq f (x:xs) = f x >> mSeq f xs

main :: IO ()
main = do
  (inputFile:outputFile:_) <- getArgs
  inp  <- openFile inputFile ReadMode
  outp <- openFile outputFile WriteMode
  cnt <- hGetContents inp
  let statements = sequence $ map parseLine $ lines cnt
  case statements of
    Left err -> print err
    Right exprs -> runST $ proofCheck exprs
  hClose inp
  hClose outp
