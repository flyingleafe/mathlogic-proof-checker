module Main where

import Utils
import Parser
import Deduction
import System.IO
import Data.List
import qualified Data.ByteString as BS

main :: IO ()
main = ioFromCmd $ \inp outp -> do
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
