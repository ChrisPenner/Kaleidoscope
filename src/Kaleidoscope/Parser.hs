{-# LANGUAGE OverloadedStrings #-}
module Kaleidoscope.Parser where

import qualified Data.Text as T
import Text.Megaparsec hiding (many)
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import Control.Applicative
import Data.Foldable
import Data.Void
import Data.Bifunctor

import Kaleidoscope.Lexer
import Kaleidoscope.PTypes

int :: Parser Expr
int = Float . fromIntegral <$> integer

floating :: Parser Expr
floating = Float <$> double

variable :: Parser Expr
variable = Var <$> identifier

binary :: T.Text -> Op -> (Parser (Expr -> Expr -> Expr) -> t) -> t
binary s f assoc = assoc (reservedOp s >> return (BinOp f))

table :: [[Operator Parser Expr]]
table = [ [ binary "*" Times InfixL
          , binary "/" Divide InfixL]
        , [ binary "+" Plus InfixL
          , binary "-" Minus InfixL]]

expr :: Parser Expr
expr = makeExprParser  factor table

function :: Parser Expr
function = do
    reserved "def"
    name <- identifier
    args <- parens $ many variable
    body <- expr
    return $ Function name args body

extern :: Parser Expr
extern = do
    reserved "extern"
    name <- identifier
    args <- parens $ many variable
    return $ Extern name args

call :: Parser Expr
call = do
    name <- identifier
    args <- parens $ commaSep expr
    return $ Call name args

factor :: Parser Expr
factor = asum
  [ try floating
  , try int
  , try extern
  , try function
  , try call
  , variable
  , parens expr
  ]

defn :: Parser Expr
defn = try extern
   <|> try function
   <|> expr

contents :: Parser a -> Parser a
contents p = do
    space
    r <- p
    eof
    return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    return def

parseExpr :: T.Text -> Either String Expr
parseExpr s = first errorBundlePretty $ parse (contents expr) "<stdin>" s

parseToplevel :: T.Text -> Either String [Expr]
parseToplevel s = first errorBundlePretty $ parse (contents toplevel) "<stdin>" s
