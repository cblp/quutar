{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

import           GHC.Generics    (Generic)
import           Options.Generic (ParseRecord, getRecord)
import           Web.Scotty      (get, html, scotty)

newtype Options = Options{port :: Int}
  deriving (Generic, ParseRecord)

main :: IO ()
main = do
  Options{port} <- getRecord "A simple web server"
  putStrLn $ "Listening at http://localhost:" ++ show port
  scotty port $
    get "/" $ do
      html "<form><input name=expr></form>"
