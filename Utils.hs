module Utils where

import System.IO
import System.Environment
import Data.Char
import qualified Data.Set as S
import Control.Monad

lowercase :: String -> String
lowercase = map toLower

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (a, b) = (f a, f b)

pairM :: Monad m => (m a, m b) -> m (a, b)
pairM (f, s) = do
  a <- f
  b <- s
  return (a, b)

concatSeq :: Monad m => [m [a]] -> m [a]
concatSeq = liftM concat . sequence

fastNub :: Ord a => [a] -> [a]
fastNub = go S.empty
  where go _ [] = []
        go s (x:xs) | S.member x s = go s xs
                    | otherwise    = x : go (S.insert x s) xs

pairs :: [a] -> Maybe [(a, a)]
pairs [] = Just []
pairs [_] = Nothing
pairs (x:y:xs) = fmap ((x, y) :) $ pairs xs

maybeToEither :: Maybe a -> e -> Either e a
maybeToEither Nothing e = Left e
maybeToEither (Just a) _ = Right a

bracketed :: Show a => (a -> t -> Bool) -> a -> t -> [Char]
bracketed comp a e
    | a `comp` e = "(" ++ show a ++ ")"
    | otherwise = show a

ioFromCmd :: (Handle -> Handle -> IO ()) -> IO ()
ioFromCmd action = do
  (inputFile:outputFile:_) <- getArgs
  inp  <- openFile inputFile ReadMode
  outp <- openFile outputFile WriteMode
  action inp outp
  hClose inp
  hClose outp
