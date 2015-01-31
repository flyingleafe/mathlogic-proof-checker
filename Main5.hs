module Main where

import Parser()
import Utils
import Topology
import System.IO
import Data.Map (toList)

main :: IO ()
main = ioFromCmd $ \inp outp -> do
  input <- hGetLine inp
  let expr = read input
      unex = makeUnexample expr
  case unex of
   Nothing -> hPutStrLn outp "Формула общезначима"
   Just mp -> hPutStrLn outp $ unlines $ map (showSet . snd) $ toList mp
