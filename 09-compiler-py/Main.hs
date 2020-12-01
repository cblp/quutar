{-# LANGUAGE QuasiQuotes #-}

import           Prelude hiding (filter)

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           NeatInterpolation (text)

data Program = Program Filter Aggregate

data Filter
  = FilterString String
  | FilterInt Int

data Aggregate = Sum | Min | Max

main :: IO ()
main = compile "filteragg.py" $ Program (FilterString "foo") Min

compile :: FilePath -> Program -> IO ()
compile file prog = do
  Text.writeFile file $ genPy prog

genPy :: Program -> Text
genPy (Program filter aggregate) =
  [text|
    import sys

    $aggregateDeclarations

    for line in sys.stdin:
        key, value = line.split()
        if $filterCondition:
            $aggregateStatements

    print($aggregateResult)
  |]
  where
    FilterCode   {..} = compileFilter    filter
    AggregateCode{..} = compileAggregate aggregate

newtype FilterCode = FilterCode
  { filterCondition :: Text
  }

compileFilter :: Filter -> FilterCode
compileFilter = \case
  FilterString s -> FilterCode{filterCondition = "key == " <> tshow s}
  FilterInt    i -> FilterCode{filterCondition = "key == " <> tshow i}

data AggregateCode = AggregateCode
  { aggregateDeclarations :: Text
  , aggregateStatements   :: Text
  , aggregateResult       :: Text
  }

compileAggregate :: Aggregate -> AggregateCode
compileAggregate = \case
  Sum ->
    AggregateCode
      { aggregateDeclarations = "sum = 0"
      , aggregateStatements   = "sum += value"
      , aggregateResult       = "sum"
      }
  Min ->
    AggregateCode
      { aggregateDeclarations = "min = None"
      , aggregateStatements   =
          [text|
            if min is None or value < min:
                min = value
          |]
      , aggregateResult       = "min"
      }
  Max ->
    AggregateCode
      { aggregateDeclarations = "max = None"
      , aggregateStatements   =
          [text|
            if max is None or value > max:
                max = value
          |]
      , aggregateResult       = "max"
      }

tshow :: Show a => a -> Text
tshow = Text.pack . show
