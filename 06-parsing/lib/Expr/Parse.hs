module Expr.Parse (expr) where

import           Prelude                        hiding (cos, div, sin)

import           Control.Applicative            ((<|>))
import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Data.Foldable                  (asum)
import           Data.Functor                   (($>))
import           Data.Void                      (Void)
import           Text.Megaparsec                (Parsec, try)
import           Text.Megaparsec.Char           (char, space,
                                                --  string
                                                 )
import           Text.Megaparsec.Char.Lexer     (decimal, float, signed)

import           Expr                           (Expr (..))

type Parser = Parsec Void String

expr :: Parser Expr
expr =
  makeExprParser
    term
    [ [Postfix $ try pow]
    , [InfixL $ try add]
    ]

term :: Parser Expr
term = space *> asum [number, var {- , expr в скобках -}]

number :: Parser Expr
number = Number <$> signed space (try float <|> decimal)

integer :: Parser Integer
integer = signed space decimal

var :: Parser Expr
var = space *> char 'x' $> Var

pow :: Parser (Expr -> Expr)
pow = do
  space
  _ <- char '^'
  space
  n <- integer
  pure $ \e -> Pow e n

add :: Parser (Expr -> Expr -> Expr)
add = space *> char '+' $> Add
