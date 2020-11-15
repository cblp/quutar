{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Prelude             hiding (id)

import           Data.Foldable       (toList)
import           Data.IORef          (IORef, modifyIORef, newIORef, readIORef)
import           Data.Sequence       (Seq, (|>))
import           Data.Text           (Text)
import           Hedgehog            (Property, evalIO, property, withTests,
                                      (===))
import           System.IO.Temp      (withSystemTempFile)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH       (defaultMainGenerator)

import           Auction             (auction)
import           Telegram            (Chat (..), Message (..), Telegram (..),
                                      Update (..), User (..))

main :: IO ()
main = $(defaultMainGenerator)

prop_auction_takes_stakes :: Property
prop_auction_takes_stakes =
  withTests 1 $
  property $ do
    sent <-
      evalIO $
        runTelegram
          [ makeUpdate 30 "Floyd" "100"
          , makeUpdate 31 "Lloyd" "200"
          , makeUpdate 32 "Floyd" "300"
          ]
    toList sent
      === [ (36, "Floyd: 100\n")
          , (36, "Floyd: 100\nLloyd: 200\n")
          , (36, "Floyd: 300\nLloyd: 200\n")
          ]

runTelegram :: [Update] -> IO (Seq (Integer, Text))
runTelegram updates = do
  (telegram, sentRef) <- newTelegram updates
  withSystemTempFile "stakes-sqlite" $ \databaseFile _ ->
    auction telegram databaseFile Nothing
  readIORef sentRef

newTelegram :: [Update] -> IO (Telegram, IORef (Seq (Integer, Text)))
newTelegram updates =
  do
    sentRef <- newIORef mempty
    let sendMessage chatId txt = modifyIORef sentRef (|> (chatId, txt))
    pure (Telegram{getUpdates, sendMessage, putLog}, sentRef)
  where
    getUpdates _ = pure updates
    putLog _ = pure ()

makeUpdate :: Integer -> Text -> Text -> Update
makeUpdate update_id first_name text = Update{update_id, message}
  where
    chat = Chat{id = 36}
    from = User{first_name, username = Nothing}
    message = Message{chat, text = Just text, from}
