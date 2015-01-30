module Utils where

import System.IO
import System.Environment
import Data.Char
import qualified Data.Set as S
import Control.Monad
import Data.List

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

orElse :: Monad m => Maybe a -> m (Maybe a) -> m (Maybe a)
orElse (Just e) _ = return $ Just e
orElse Nothing s = s

fastNub :: Ord a => [a] -> [a]
fastNub = go S.empty
  where go _ [] = []
        go s (x:xs) | S.member x s = go s xs
                    | otherwise    = x : go (S.insert x s) xs

unionMap :: Ord b => (a -> [b]) -> [a] -> [b]
unionMap f = fastNub . concatMap f

unite :: Ord a => [[a]] -> [a]
unite = unionMap id

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf a b = a \\ b == []

pairs :: [a] -> Maybe [(a, a)]
pairs [] = Just []
pairs [_] = Nothing
pairs (x:y:xs) = fmap ((x, y) :) $ pairs xs

maybeToEither :: Maybe a -> e -> Either e a
maybeToEither Nothing e = Left e
maybeToEither (Just a) _ = Right a

fromEither :: Either a b -> b
fromEither (Right b) = b
fromEither (Left _) = error "Invalid get from either"

bracketed :: Show a => (a -> t -> Bool) -> a -> t -> [Char]
bracketed comp a e
    | a `comp` e = "(" ++ show a ++ ")"
    | otherwise = show a

showCtx :: Show a => [a] -> a -> String
showCtx c d = (intercalate ", " . map show) c ++ "|-" ++ show d

ioFromCmd :: (Handle -> Handle -> IO ()) -> IO ()
ioFromCmd action = do
  (inputFile:outputFile:_) <- getArgs
  inp  <- openFile inputFile ReadMode
  outp <- openFile outputFile WriteMode
  action inp outp
  hClose inp
  hClose outp
