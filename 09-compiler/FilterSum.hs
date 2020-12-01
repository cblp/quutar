main :: IO ()
main = do
  fileContent <- getContents
  print $ filterSum "42" $ map readRecord $ lines fileContent

readRecord :: String -> (String, Integer)
readRecord s =
  case words s of
    i1 : i2 : _ -> (i1, read i2)
    _           -> error "there must be at least 2 fields in a row"

filterSum :: String -> [(String, Integer)] -> Integer
filterSum x = sum . map snd . filter (\(x', _) -> x' == x)
