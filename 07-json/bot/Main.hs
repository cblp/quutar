module Main where

import           Control.Exception  (catchJust)
import           Control.Monad      (guard)
import           System.Environment (getEnv)
import           System.IO.Error    (isDoesNotExistError)
import           Text.Read          (readMaybe)

import           Auction            (auction)
import           Telegram           (production)

main :: IO ()
main = do
  token     <- getEnv "TELEGRAM_BOT_TOKEN"
  updateIdM <- loadUpdateId
  auction (Telegram.production token) "database.sqlite" updateIdM

loadUpdateId :: IO (Maybe Integer)
loadUpdateId =
  catchJust
    (guard . isDoesNotExistError)
    (readMaybe <$> readFile "update_id")
    (\_ -> pure Nothing)
