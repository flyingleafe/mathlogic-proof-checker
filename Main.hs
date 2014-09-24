{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import Prelude hiding (unlines, concat)
import Proof
import Parser
import System.IO hiding (hGetContents, hPutStrLn)
import System.Environment
import Control.Monad.ST
import Data.ByteString.Char8 hiding (map)

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
  let statements = parseProof cnt
  case statements of
    Left err -> hPrint outp err
    Right exprs -> do
      let checked = runST $ proofCheck [] exprs
          showChecked (i, l, an) = concat ["(", pack $ show i, ") ",
                                           pack $ show l,
                                           " (", pack $ show an,  ")"]
         in hPutStrLn outp $ unlines . map showChecked $ checked
  hClose inp
  hClose outp
