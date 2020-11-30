{-# LANGUAGE QuasiQuotes #-}

import           Prelude hiding (filter)

import qualified Language.C.Quote as C
import           Language.C.Quote.GCC (cdecl, cexp, cunit)
import           System.Process (callProcess)
import           Text.PrettyPrint.Mainland (pretty)
import           Text.PrettyPrint.Mainland.Class (ppr)

data Program = Program Filter Mapper Reducer

data Filter = Filter
  { value :: FilterValue
  -- TODO , filterField :: Word
  }

data FilterValue
  = FilterString String
  | FilterInt Int

data Mapper = Mapper
  -- { mapperField :: Word
  -- }

data Reducer = Sum

main :: IO ()
main =
  compileAndWriteFile "filtersum.c" $
    Program
      Filter{ {- field = 1 -} value = FilterInt 33}
      Mapper{ {- field = 2 -} }
      Sum

compileAndWriteFile :: FilePath -> Program -> IO ()
compileAndWriteFile file prog = do
  writeFile file $ pretty 80 $ ppr $ compile prog
  callProcess
    "gcc" [file, "-Wall", "-Werror", "-Wextra", "-pedantic"]

compile :: Program -> [C.Definition]
compile (Program filter Mapper Sum) =
  [cunit|
    $esc:("#include <stdint.h>")
    $esc:("#include <stdio.h>")
    $esc:("#include <string.h>")

    int main() {
      typename int64_t sum = 0;
      $decl:declaration;
      typename int64_t f2;
      while (scanf($string:scanfSpec, $scanfRef, &f2) == 2) {
        if ($condition)
          sum += f2;
      }
      printf("%lld\n", sum);
      return 0;
    }
  |]
  where
    FilterCode{declaration, scanfSpec, scanfRef, condition} =
      compileFilter filter

data FilterCode = FilterCode
  { declaration :: C.InitGroup
  , scanfSpec   :: String
  , scanfRef    :: C.Exp
  , condition   :: C.Exp
  }

compileFilter :: Filter -> FilterCode
compileFilter Filter{value} =
  case value of
    FilterString s ->
      FilterCode
        { declaration = [cdecl| char f1[16]; |]
        , scanfSpec   = "%s%lld"
        , scanfRef    = [cexp| f1 |]
        , condition   = [cexp| strcmp(f1, $string:s) == 0 |]
        }
    FilterInt i ->
      FilterCode
        { declaration = [cdecl| typename int64_t f1; |]
        , scanfSpec   = "%lld%lld"
        , scanfRef    = [cexp| &f1 |]
        , condition   = [cexp| f1 == $int:i |]
        }
