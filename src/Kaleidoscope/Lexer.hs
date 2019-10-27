{-# LANGUAGE OverloadedStrings #-}
module Kaleidoscope.Lexer where

import Prelude hiding (lex)
import qualified Data.Text as T
import Data.Void
import Control.Applicative hiding (many)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

type Parser a = Parsec Void T.Text a

lex :: Parser a -> Parser a
lex = lexeme space

sym :: T.Text -> Parser T.Text
sym = symbol space

integer :: Parser Int
integer = lex decimal

double :: Parser Double
double = lex float

identifier :: Parser T.Text
identifier = T.pack <$> lex (liftA2 (:) lowerChar (many alphaNumChar))

parens :: Parser a -> Parser a
parens = between (sym "(") (sym ")")

semiSep :: Parser a -> Parser [a]
semiSep p = p `sepBy` (sym ";")

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` (sym ",")

reserved :: T.Text -> Parser T.Text
reserved = sym

reservedOp :: T.Text -> Parser T.Text
reservedOp = sym
