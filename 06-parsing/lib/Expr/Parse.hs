module Expr.Parse (expr) where

import           Control.Applicative        ((<|>))
import           Data.Foldable              (asum)
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, try)
import           Text.Megaparsec.Char       (char, space)
import           Text.Megaparsec.Char.Lexer (decimal, float, signed)

import           Expr                       (Expr (..))

type Parser = Parsec Void String

expr :: Parser Expr
expr =
  asum
    [ try number
    , try var
    ]

number :: Parser Expr
number = Number <$> signed space (try float <|> decimal)

var :: Parser Expr
var = Var <$ char 'x'
