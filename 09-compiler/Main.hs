{-# LANGUAGE QuasiQuotes #-}

import           Prelude hiding (filter)

import qualified Language.C.Quote as C
import           Language.C.Quote.GCC (cdecl, cexp, cstms, cunit)
import           System.Process (callProcess)
import           Text.PrettyPrint.Mainland (pretty)
import           Text.PrettyPrint.Mainland.Class (ppr)

data Program = Program Filter Aggregate

data Filter
  = FilterString String
  | FilterInt Int

data Aggregate = Sum | Min | Max

main :: IO ()
main = compile "filtersum.c" $ Program (FilterInt 33) Min

compile :: FilePath -> Program -> IO ()
compile file prog = do
  writeFile file $ pretty 80 $ ppr $ genC prog
  callProcess "gcc" [file, "-Wall", "-Werror", "-Wextra", "-pedantic"]

genC :: Program -> [C.Definition]
genC (Program filter aggregate) =
  [cunit|
    $esc:("#include <stdint.h>")
    $esc:("#include <stdio.h>")
    $esc:("#include <string.h>")

    int main() {
      $decl:filterDeclaration;
      $decl:aggregateDeclaration;
      typename int64_t value;
      while (
        scanf($string:(filterScanfSpec ++ "%lld"), $filterScanfRef, &value) == 2
      ) {
        if ($filterCondition) {
          $stms:aggregateStatements
        }
      }
      printf("%lld\n", $aggregateResult);
      return 0;
    }
  |]
  where
    FilterCode   {..} = compileFilter    filter
    AggregateCode{..} = compileAggregate aggregate

data FilterCode = FilterCode
  { filterDeclaration :: C.InitGroup
  , filterScanfSpec   :: String
  , filterScanfRef    :: C.Exp
  , filterCondition   :: C.Exp
  }

compileFilter :: Filter -> FilterCode
compileFilter = \case
  FilterString s ->
    FilterCode
      { filterDeclaration = [cdecl| char key[16]; |]
      , filterScanfSpec   = "%s"
      , filterScanfRef    = [cexp| key |]
      , filterCondition   = [cexp| strcmp(key, $string:s) == 0 |]
      }
  FilterInt i ->
    FilterCode
      { filterDeclaration = [cdecl| typename int64_t key; |]
      , filterScanfSpec   = "%lld"
      , filterScanfRef    = [cexp| &key |]
      , filterCondition   = [cexp| key == $int:i |]
      }

data AggregateCode = AggregateCode
  { aggregateDeclaration :: C.InitGroup
  , aggregateStatements  :: [C.Stm]
  , aggregateResult      :: C.Exp
  }

compileAggregate :: Aggregate -> AggregateCode
compileAggregate = \case
  Sum ->
    AggregateCode
      { aggregateDeclaration = [cdecl| typename int64_t result = 0; |]
      , aggregateStatements  = [cstms| result += value; |]
      , aggregateResult      = [cexp| result |]
      }
  Min ->
    AggregateCode
      { aggregateDeclaration = [cdecl| typename int64_t result = INT64_MAX; |]
      , aggregateStatements  =
          [cstms|
            if (value < result)
              result = value;
          |]
      , aggregateResult      = [cexp| result |]
      }
  Max ->
    AggregateCode
      { aggregateDeclaration = [cdecl| typename int64_t result = INT64_MIN; |]
      , aggregateStatements  =
          [cstms|
            if (value > result)
              result = value;
          |]
      , aggregateResult      = [cexp| result |]
      }
