{-# LANGUAGE OverloadedStrings #-}
module Parser where

import LogicType
import ExprType
import Utils
import Text.Parsec
import Text.Parsec.ByteString
import Text.Parsec.Expr
import Data.String
import Data.ByteString hiding (drop, foldl, foldr)
import Control.Monad
import Control.Applicative ((<$>), (<*>), (<*), (*>))

-- Parsing grammar

spaces' :: Parser ()
spaces' = skipMany $ char ' '
lexeme p = do { res <- p; spaces'; return res; }
parens p = do
  char '(' >> spaces'
  res <- p
  spaces'
  char ')' >> spaces'
  return res
symbol s = do { res <- string s; spaces'; return res; }
commaSep p = p `sepBy` symbol ","

orParser = try $ do
  symbol "|"
  notFollowedBy $ char '-'
  return (:|)

exprTable = [ [binary "&" (:&) AssocLeft]
            , [Infix orParser AssocLeft]
            , [binary "->" (:->) AssocRight]]

binary name fun assoc = Infix (symbol name >> return fun) assoc

expr :: Parser Logic
expr = buildExpressionParser exprTable tok

proof = many $ expr <* optional endOfLine

tok :: Parser Logic
tok = try predicate <|> denial <|> parens expr <|> quantor

denial :: Parser Logic
denial = symbol "!" >> tok >>= return . Not

quantor :: Parser Logic
quantor = do
  tp <- (symbol "@" >> return Forall) <|> (symbol "?" >> return Exists)
  v <- vname
  e <- tok
  return $ Quant tp v e

predicate :: Parser Logic
predicate = namedPred <|> eqPred

namedPred :: Parser Logic
namedPred = do
  name <- lexeme $ (:) <$> upper <*> many digit
  exprs <- parens (commaSep arith) <|> return []
  return $ Pred (VarID name exprs)

eqPred = do
  frst <- arith
  symbol "="
  scnd <- arith
  return $ Pred (VarID "=" [frst, scnd])

arithTable = [ [binary "*" Mul AssocLeft]
             , [binary "+" Add AssocLeft]]

arith = buildExpressionParser arithTable succ'

succ' = do
  core <- mult
  ticks <- many $ char '\''
  return $ foldr (\_ a -> Succ a) core ticks

mult :: Parser Expr
mult = try func <|> var <|> parens arith <|> (symbol "0" >> return Zero)

vname = lexeme $ (:) <$> lower <*> many digit

func = do
  name <- vname
  exprs <- parens $ commaSep arith
  return $ Func name exprs

var = vname >>= return . Var

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
  cs <- commaSep expr
  symbol "|-"
  e <- expr
  return (cs, e)

parseDerivability :: ByteString -> Either ParseError ([Logic], Logic)
parseDerivability = parse derivParser ""

parseProofWithHeading = parse (pairM (derivParser <* endOfLine, proof)) ""

parseT :: Monad m => Parser a -> (String, Int, Int) -> ByteString -> m a
parseT p (file, line, col) s =
  case parse p' "" s of
    Left err -> fail $ show err
    Right e -> return e
    where p' = do updatePosition file line col
                  spaces'
                  e <- lexeme p
                  eof
                  return e

updatePosition :: Monad m => String -> Int -> Int -> ParsecT s u m ()
updatePosition file line col = do
   pos <- getPosition
   setPosition $
     (flip setSourceName) file $
     (flip setSourceLine) line $
     (flip setSourceColumn) col $
     pos
