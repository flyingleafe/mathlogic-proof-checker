module Main where

import Utils
import Parser
import Deduction
import Proof
import System.IO
import Data.List
import qualified Data.ByteString as BS

outputResult :: Handle -> (Heading, Proof) -> IO ()
outputResult outp ((ctx, derived), newProof) = do
  hPutStrLn outp $ showCtx ctx derived
  hPutStrLn outp $ unlines . map show $ newProof

outputError :: Show a => Handle -> a -> IO ()
outputError outp err = hPutStrLn outp $ show err

main :: IO ()
main = ioFromCmd $ \inp outp -> do
  cnt <- BS.hGetContents inp
  let parsed = parseProofWithHeading cnt
  case parsed of
   Left err -> outputError outp err
   Right (heading, oldProof) ->
     either (outputError outp) (outputResult outp) $ applyDeduction heading oldProof
