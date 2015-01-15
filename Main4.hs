module Main where

import Utils
import Parser
import Proof
import Deduction
import System.IO
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = ioFromCmd $ \inp outp -> do
  input <- BS.hGetContents inp
  let ph = parseProofWithHeading input
  case ph of
   Left err -> hPutStrLn outp $ show err
   Right (heading, proof) ->
     either outErr outRes $ applyDeduction heading proof
       where outErr (i, Fatal) = hPutStrLn outp $
                                   "Вывод некорректен, начиная с формулы №" ++ show i
             outErr (i, err) = hPutStrLn outp $
                               "Вывод некорректен, начиная с формулы №" ++
                               show i ++ ": " ++ show err
             outRes ((ctx, derived), newProof) = do
               hPutStrLn outp $ showCtx ctx derived
               hPutStrLn outp $ unlines . map show $ newProof
