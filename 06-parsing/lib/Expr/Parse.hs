module Expr.Parse (expr) where

import           Prelude hiding (cos, div, sin)

import           Control.Applicative ((<|>))
import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Data.Foldable (asum)
import           Data.Void (Void)
import           Text.Megaparsec (Parsec, try)
import           Text.Megaparsec.Char (char, space)
                                      -- string,
import           Text.Megaparsec.Char.Lexer (decimal, float, signed)

import           Expr (Expr (..))

type Parser = Parsec Void String

expr :: Parser Expr
expr =
  makeExprParser
    term
    [ [Postfix pow]
    , [InfixL add]
    ]

term :: Parser Expr
term = asum [number, var {- , expr в скобках -}]

number :: Parser Expr
number = Number <$> signed space (try float <|> decimal) <* space

integer :: Parser Integer
integer = signed space decimal <* space

var :: Parser Expr
var = Var <$ char 'x' <* space

pow :: Parser (Expr -> Expr)
pow = do
  _ <- char '^' <* space
  n <- integer
  pure $ \e -> Pow e n

add :: Parser (Expr -> Expr -> Expr)
add = Add <$ char '+' <* space
