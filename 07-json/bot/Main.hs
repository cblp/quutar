{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude            hiding (id)

import           Control.Exception  (catchJust)
import           Control.Lens       ((^.))
import           Control.Monad      (guard, void)
import           Data.Aeson         as Json
import           Data.Foldable      (for_, toList)
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Data.Time          (getCurrentTime)
import           Database.Persist   (Entity (..), selectList, upsert, (=.))
import           GHC.Generics       (Generic)
import           Network.Wreq       (FormParam ((:=)), asJSON, post,
                                     responseBody)
import           System.Environment (getEnv)
import           System.IO          (hPutStr, stderr)
import           System.IO.Error    (isDoesNotExistError)
import           Text.Read          (readMaybe)

import           Database           (EntityField (StakeText), Stake (..), runDb)

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
          void $
            upsert  -- update or insert
              (Stake username' text')  -- insert
              [StakeText =. text']  -- update
          map entityVal <$> selectList [] []
      void $ sendMessage token chatId $
        Text.unlines
          [ stakeUsername <> ": " <> stakeText
          | Stake{stakeUsername, stakeText} <- stakes
          ]
      putLog $ "Sent results to " ++ show chat ++ " " ++ show from

loadUpdateId :: IO (Maybe Integer)
loadUpdateId =
  catchJust
    (guard . isDoesNotExistError)
    (readMaybe <$> readFile "update_id")
    (\_ -> pure Nothing)

type Token = String

data Update = Update
  { update_id :: Integer
  , message   :: Message
  }
  deriving (FromJSON, Generic)

-- instance FromJSON Update where
--   parseJSON =
--     withObject "Update" $
--       \obj -> do
--         update_id <- obj .: "update_id"
--         message   <- obj .: "message"
--         pure Update{update_id, message}

data Message = Message
  { chat :: Chat
  , text :: Maybe Text
  , from :: User
  }
  deriving (FromJSON, Generic, Show)

data User = User
  { first_name :: Text
  , username   :: Maybe Text
  }
  deriving (FromJSON, Generic, Show)

data Chat = Chat
  { id :: Integer
  }
  deriving (FromJSON, Generic, Show)

data Ok a = Ok
  { ok     :: Bool
  , result :: a
  }
  deriving (FromJSON, Generic)

-- instance FromJSON Bool...

-- instance FromJSON a => FromJSON (Ok a) where
--   parseJSON =
--     withObject "Ok" $
--       \obj -> do
--         ok     <- obj .: "ok"
--         result <- obj .: "result"
--         pure Ok{ok, result}

--           case HashMap.lookup "ok" obj of
--             Nothing -> fail "no key 'ok'"
--             Just v ->
--               -- case v of
--               --   Json.Bool b -> pure b
--               --   _           -> fail "not a bool"
--               parseJSON v

getUpdates :: Token -> Maybe Integer -> IO [Update]
getUpdates token lastUpdateIdM =
  telegramRpc token "getUpdates" params
  where
    params =
      ("timeout" := (20 :: Integer))
      : ["offset" := updateId + 1 | updateId <- toList lastUpdateIdM]

sendMessage :: Token -> Integer -> Text -> IO Message
sendMessage token chat_id text =
  telegramRpc token "sendMessage" ["chat_id" := chat_id, "text" := text]

telegramRpc :: FromJSON result => Token -> String -> [FormParam] -> IO result
telegramRpc token method params = do
  rawResponse <-
    post ("https://api.telegram.org/bot" <> token <> "/" <> method) params
  jsonResponse <- asJSON rawResponse
  let Ok{ok, result} = jsonResponse ^. responseBody
  guard ok
  pure result

putLog :: String -> IO ()
putLog message = do
  time <- getCurrentTime
  hPutStr stderr $ show time ++ ' ' : message ++ "\n"
