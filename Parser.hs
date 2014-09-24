{-# LANGUAGE OverloadedStrings #-}
module Parser where

import LogicType
import Utils
import Text.Parsec
import Text.Parsec.ByteString
import Text.Parsec.Expr
import Data.String
import Data.ByteString hiding (drop)

-- Parsing grammar

lexeme p = do { res <- p; spaces; return res; }
parens p = do
  char '(' >> spaces
  res <- p
  spaces
  char ')' >> spaces
  return res
symbol s = do { res <- string s; spaces; return res; }

optMaybe :: Parser a -> Parser (Maybe a)
optMaybe = optionMaybe

orParser = try $ do
  symbol "|"
  notFollowedBy $ char '-'
  return Or

exprTable = [ [binary "&" And AssocLeft]
            , [Infix orParser AssocLeft]
            , [binary "->" Cons AssocRight]]

binary name fun assoc = Infix (symbol name >> return fun) assoc

expr :: Parser Logic
expr = buildExpressionParser exprTable tok

proof = many expr

denial :: Parser Logic
denial = do
  symbol "!"
  t <- tok
  return $ Not t

tok :: Parser Logic
tok = parens expr <|> denial <|> do
  var <- lexeme $ many1 alphaNum
  return $ Var var

parseProof :: ByteString -> Either ParseError [Logic]
parseProof = parse proof ""

parseForRead :: ByteString -> Either ParseError (SourcePos, Logic)
parseForRead = parse tokenExpr ""
    where tokenExpr = do
            e <- expr
            p <- getPosition
            return (p, e)

-- Useful instances
instance Read Logic where
  readsPrec _ val = case parseForRead (fromString val) of
    Left _ -> []
    Right (p, e) -> [(e, drop (sourceColumn p) val)]

-- Derivability parsing
derivParser :: Parser ([Logic], Logic)
derivParser = do
  cs <- expr `sepBy` symbol ","
  symbol "|-"
  e <- expr
  return (cs, e)

parseDerivability :: ByteString -> Either ParseError ([Logic], Logic)
parseDerivability = parse derivParser ""

parseProofWithHeading = parse (pairM (derivParser, proof)) ""
