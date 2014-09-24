{-# LANGUAGE OverloadedStrings #-}
module Main where

import Proof
import Parser
import System.IO
import System.Environment
import Control.Monad.ST
import qualified Data.ByteString as BS

mSeq :: Monad m => (a -> m b) -> [a] -> m b
mSeq _ [] = error "Empty list"
mSeq f [x] = f x
mSeq f (x:xs) = f x >> mSeq f xs

main :: IO ()
main = do
  (inputFile:outputFile:_) <- getArgs
  inp  <- openFile inputFile ReadMode
  outp <- openFile outputFile WriteMode
  cnt <- BS.hGetContents inp
  let statements = parseProof cnt
  case statements of
    Left err -> hPrint outp err
    Right exprs -> do
      let checked = runST $ proofCheck [] exprs
          showChecked (i, l, an) = "(" ++ show i ++ ") "
                                   ++  show l ++
                                   " (" ++  show an ++  ")"
         in hPutStrLn outp $ unlines . map showChecked $ checked
  hClose inp
  hClose outp
