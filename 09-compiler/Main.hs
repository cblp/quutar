import           Prelude hiding (filter)

import qualified Language.C.Quote as C
import           Language.C.Quote.GCC (cdecl, cexp, cunit)
import           System.Process (callProcess)
import           Text.PrettyPrint.Mainland (pretty)
import           Text.PrettyPrint.Mainland.Class (ppr)

data Program = Program
  { filter  :: Filter
  , mapper  :: Mapper
  , reducer :: Reducer
  }

data Filter = Filter
  { filterValue :: FilterValue
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
      (Filter {- 1 -} $ FilterInt 33)
      (Mapper {- 2 -})
      Sum

compileAndWriteFile :: FilePath -> Program -> IO ()
compileAndWriteFile file prog = do
  writeFile file $ pretty 80 $ ppr $ compile prog
  callProcess
    "gcc" [file, "-Wall", "-Werror", "-Wextra", "-pedantic"]

compile :: Program -> [C.Definition]
compile Program{filter = Filter{filterValue}} =
  [cunit|
    $esc:("#include <stdint.h>")
    $esc:("#include <stdio.h>")
    $esc:("#include <string.h>")

    int main() {
      typename int64_t sum = 0;
      $decl:decl1;
      typename int64_t f2;
      while (scanf($string:scanSpec1, $scanArg1, &f2) == 2) {
        if ($filterCond)
          sum += f2;
      }
      printf("%lld\n", sum);
      return 0;
    }
  |]
  where
    (decl1, scanSpec1, scanArg1, filterCond) =
      case filterValue of
        FilterString s ->
          ( [cdecl| char f1[16]; |]
          , "%s%lld"
          , [cexp| f1 |]
          , [cexp| strcmp(f1, $string:s) == 0 |]
          )
        FilterInt i ->
          ( [cdecl| typename int64_t f1; |]
          , "%lld%lld"
          , [cexp| &f1 |]
          , [cexp| f1 == $int:i |]
          )
