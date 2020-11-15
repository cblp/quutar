module Main where

import           System.Environment (getEnv)

import           Auction            (Bot (..), auction)
import           Telegram           (production)

main :: IO ()
main = do
  token <- getEnv "TELEGRAM_BOT_TOKEN"
  auction
    (Telegram.production token)
    Bot{databaseFile = "database.sqlite", updateIdFile = "update_id.txt"}
