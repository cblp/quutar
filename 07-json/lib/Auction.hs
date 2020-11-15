{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Auction (Bot (..), auction) where

import           Prelude           hiding (id)

import           Control.Exception (catchJust)
import           Control.Monad     (guard, void)
import           Data.Foldable     (for_)
import           Data.Maybe        (fromMaybe)
import           Data.Text         (Text)
import qualified Data.Text         as Text
import           Database          (EntityField (StakeValue), Stake (..), runDb)
import           Database.Persist  (entityVal, selectList, upsert, (=.))
import           System.IO.Error   (isDoesNotExistError)
import           Telegram          (Chat (..), Message (..), Telegram (..),
                                    Update (..), User (..))
import           Text.Read         (readMaybe)

data Bot = Bot
  { databaseFile :: FilePath
  , updateIdFile :: FilePath
  }

auction :: Telegram -> Bot -> IO ()
auction telegram@Telegram{getUpdates, putLog} bot@Bot{updateIdFile} = do
  updateIdM <- loadUpdateId updateIdFile
  putLog "Waiting for updates..."
  updates <- getUpdates updateIdM
  putLog $ "Got " <> show (length updates) <> " updates"
  for_ updates $ handleUpdate telegram bot

loadUpdateId :: FilePath -> IO (Maybe Integer)
loadUpdateId updateIdFile =
  catchJust
    (guard . isDoesNotExistError)
    (readMaybe <$> readFile updateIdFile)
    (\_ -> pure Nothing)

handleUpdate :: Telegram -> Bot -> Update -> IO ()
handleUpdate telegram bot Update{update_id, message} =
  do
    case text of
      Just text' -> handleNewStake text'
      Nothing    -> putLog $ "No stake for " ++ show message
    writeFile updateIdFile $ show update_id
    putLog $ "Written update_id = " ++ show update_id
  where

    Telegram{sendMessage, putLog} = telegram
    Bot{databaseFile, updateIdFile} = bot

    Message{chat, text, from} = message
    Chat{id = chatId} = chat
    User{first_name, username} = from

    username' = fromMaybe first_name username
      -- case username of
      --   Just username' -> username'
      --   Nothing        -> first_name

    handleNewStake text' = do
      stakes <-
        runDb (Text.pack databaseFile) $ do
          let value = read $ Text.unpack text'
          void $
            upsert  -- update or insert
              (Stake username' value)  -- insert
              [StakeValue =. value]  -- update
          map entityVal <$> selectList [] []
      void $ sendMessage chatId $
        Text.unlines
          [ stakeUsername <> ": " <> tshow stakeValue
          | Stake{stakeUsername, stakeValue} <- stakes
          ]
      putLog $ "Sent results to " ++ show chat ++ " " ++ show from

tshow :: Show a => a -> Text
tshow = Text.pack . show
