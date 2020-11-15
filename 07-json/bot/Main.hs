{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude            hiding (id)

import           Control.Exception  (catchJust)
import           Control.Monad      (guard, void)
import           Data.Foldable      (for_)
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Data.Time          (getCurrentTime)
import           Database.Persist   (entityVal, selectList, upsert, (=.))
import           System.Environment (getEnv)
import           System.IO          (hPutStr, stderr)
import           System.IO.Error    (isDoesNotExistError)
import           Text.Read          (readMaybe)

import           Database           (EntityField (StakeValue), Stake (..),
                                     runDb)
import           Telegram           (Chat (..), Message (..), Token,
                                     Update (..), User (..), getUpdates,
                                     sendMessage)

main :: IO ()
main = do
  token <- getEnv "TELEGRAM_BOT_TOKEN"
  updateIdM <- loadUpdateId

  putLog "Waiting for updates..."
  updates <- getUpdates token updateIdM
  putLog $ "Got " <> show (length updates) <> " updates"

  for_ updates $ handleUpdate token

handleUpdate :: Token -> Update -> IO ()
handleUpdate token Update{update_id, message} =
  do
    case text of
      Just text' -> handleNewStake text'
      Nothing    -> putLog $ "No stake for " ++ show message
    writeFile "update_id" $ show update_id
    putLog $ "Written update_id = " ++ show update_id
  where

    Message{chat, text, from} = message
    Chat{id = chatId} = chat
    User{first_name, username} = from

    username' = fromMaybe first_name username
      -- case username of
      --   Just username' -> username'
      --   Nothing        -> first_name

    handleNewStake text' = do
      stakes <-
        runDb "database.sqlite" $ do
          let value = read $ Text.unpack text'
          void $
            upsert  -- update or insert
              (Stake username' value)  -- insert
              [StakeValue =. value]  -- update
          map entityVal <$> selectList [] []
      void $ sendMessage token chatId $
        Text.unlines
          [ stakeUsername <> ": " <> tshow stakeValue
          | Stake{stakeUsername, stakeValue} <- stakes
          ]
      putLog $ "Sent results to " ++ show chat ++ " " ++ show from

loadUpdateId :: IO (Maybe Integer)
loadUpdateId =
  catchJust
    (guard . isDoesNotExistError)
    (readMaybe <$> readFile "update_id")
    (\_ -> pure Nothing)

putLog :: String -> IO ()
putLog message = do
  time <- getCurrentTime
  hPutStr stderr $ show time ++ ' ' : message ++ "\n"

tshow :: Show a => a -> Text
tshow = Text.pack . show
