main :: IO ()
main = do
  fileContent <- getContents
  print $ filterSum "42" $ map readRecord $ lines fileContent

readRecord :: String -> (String, Integer)
readRecord s =
  let i1 : i2 : _ = words s
  in (i1, read i2)

filterSum :: String -> [(String, Integer)] -> Integer
filterSum x = sum . map snd . filter (\(x', _) -> x' == x)
