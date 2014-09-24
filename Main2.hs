module Main where

import Parser
import Deduction
import System.IO
import System.Environment
import Data.List
import qualified Data.ByteString as BS

main :: IO ()
main = do
  (inputFile:outputFile:_) <- getArgs
  inp  <- openFile inputFile ReadMode
  outp <- openFile outputFile WriteMode
  cnt <- BS.hGetContents inp
  let parsed = parseProofWithHeading cnt
  case parsed of
   Left err -> hPutStrLn outp $ show err
   Right (heading, oldProof) -> do
     let ((ctx, derived), newProof) = applyDeduction heading oldProof
         showCtx c d = (intercalate ", " . map show) c ++
                       " |- " ++ show d
     hPutStrLn outp $ showCtx ctx derived
     hPutStrLn outp $ unlines . map show $ newProof
  hClose inp
  hClose outp
