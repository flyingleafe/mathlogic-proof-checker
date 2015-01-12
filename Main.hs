{-# LANGUAGE OverloadedStrings #-}
module Main where

import Utils
import Proof
import Parser
import System.IO
import Control.Monad.ST
import qualified Data.ByteString as BS

main :: IO ()
main = ioFromCmd $ \inp outp -> do
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
