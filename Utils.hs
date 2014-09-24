module Utils where

pairM :: Monad m => (m a, m b) -> m (a, b)
pairM (f, s) = do
  a <- f
  b <- s
  return (a, b)
