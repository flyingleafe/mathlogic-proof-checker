{-# LANGUAGE OverloadedStrings #-}
module Parser (Logic(..)
              , firstl
              , secondl
              , parseLine
              , similar
              , similarOrGt) where

import Text.ParserCombinators.Parsec
import Data.Hashable

data Logic = Var String
           | Logic `And` Logic
           | Logic `Or` Logic
           | Not Logic
           | Logic `Cons` Logic
             deriving (Eq, Ord)

firstl :: Logic -> Logic
firstl (Var _) = error "No subexpr"
firstl (Not e) = e
firstl (Or e _) = e
firstl (And e _) = e
firstl (Cons e _) = e

secondl :: Logic -> Logic
secondl (Var _) = error "No subexpr"
secondl (Not _) = error "No subexpr"
secondl (Or _ e) = e
secondl (And _ e) = e
secondl (Cons _ e) = e

-- Parsing grammar
lexem :: Parser a -> Parser a
lexem p = do
  spaces
  x <- p
  spaces
  return x

alpha :: Parser Char
alpha = oneOf ['a'..'z']

tok :: Parser Logic
tok = var <|> parenExpr
    where var = do
            s <- many1 alpha
            return $ Var s
          parenExpr = do
            lexem $ char '('
            e <- lexem expr
            lexem $ char ')'
            return e

denial :: Parser Logic
denial =   try (do
                 char '!'
                 return . Not =<< denial)
       <|> tok

leftAssocRec :: Parser Logic -> (Logic -> Logic -> Logic) -> Char -> Logic -> Parser Logic
leftAssocRec p f sym acc = try (do
                                 lexem $ char sym
                                 end <- p
                                 leftAssocRec p f sym $ f acc end)
                           <|> return acc

conj :: Parser Logic
conj = do
  first <- denial
  leftAssocRec denial And '&' first

disj :: Parser Logic
disj = do
  first <- conj
  leftAssocRec conj Or '|' first

expr :: Parser Logic
expr = try (do
             first <- disj
             lexem $ string "->"
             second <- expr
             return $ first `Cons` second)
       <|> disj

lineexpr :: Parser Logic
lineexpr = do
  e <- expr
  eol <|> eof
  return e

eol :: Parser ()
eol = char '\n' >> return ()

parseLine :: String -> Either ParseError Logic
parseLine = parse lineexpr ""

parseForRead :: String -> Either ParseError (SourcePos, Logic)
parseForRead = parse tokenExpr ""
    where tokenExpr = do
            e <- expr
            p <- getPosition
            return (p, e)

-- Useful instances
instance Read Logic where
  readsPrec _ val = case parseForRead val of
    Left _ -> []
    Right (p, e) -> [(e, drop (sourceColumn p) val)]

brackets :: Show a => (a -> t -> Bool) -> a -> t -> [Char]
brackets comp a e
    | a `comp` e = "(" ++ show a ++ ")"
    | otherwise = show a

similar :: Logic -> Logic -> Bool
similar (Var _) (Var _) = True
similar (Not _) (Not _) = True
similar (And _ _) (And _ _) = True
similar (Or _ _) (Or _ _) = True
similar (Cons _ _) (Cons _ _) = True
similar _ _ = False

similarOrGt :: Logic -> Logic -> Bool
similarOrGt a b = similar a b || (a > b)

instance Show Logic where
  show (Var s) = s
  show (Not e) = case e of
    Var s -> '!' : s
    _ -> "!(" ++ show e ++ ")"
  show e@(a `And` b) = brackets (>) a e ++ " & " ++ brackets similarOrGt b e
  show e@(a `Or` b) = brackets (>) a e ++ " | " ++ brackets similarOrGt b e
  show e@(a `Cons` b) = brackets similarOrGt a e ++ " -> " ++ show b

instance Hashable Logic where
  hashWithSalt salt = hashWithSalt salt . show
