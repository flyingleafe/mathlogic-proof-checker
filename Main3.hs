module Main where

import LogicType
import Utils
import Parser()
import ProofBuilder
import System.IO
import Control.Monad
import Data.List
import qualified Data.ByteString.Char8 as BS

showEst :: [(String, String)] -> String
showEst = intercalate ", " . map (\(a, b) -> a ++ "=" ++ b)

main :: IO ()
main = ioFromCmd $ \inp outp -> do
  input <- BS.hGetLine inp
  let expr = read $ BS.unpack input
      proof = buildProof expr
  case proof of
   Left wr -> hPutStrLn outp $ "Высказывание ложно при " ++ showEst wr
   Right exprs -> hPutStr outp $ unlines $ map show exprs
