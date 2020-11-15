{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram
  ( Chat (..), Message (..), Token, Update (..), User (..)
  , getUpdates, sendMessage
  ) where

import           Control.Lens  ((^.))
import           Control.Monad (guard)
import           Data.Aeson    (FromJSON)
import           Data.Foldable (toList)
import           Data.Text     (Text)
import           GHC.Generics  (Generic)
import           Network.Wreq  (FormParam ((:=)), asJSON, post, responseBody)

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
