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
import           System.FilePath     ((</>))
import           System.IO.Temp      (withSystemTempDirectory)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH       (defaultMainGenerator)

import           Auction             (Bot (..), auction)
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
          [ makeUpdate "Floyd" "100"
          , makeUpdate "Lloyd" "200"
          , makeUpdate "Floyd" "300"
          ]
    toList sent
      === [ "Floyd: 100\n"
          , "Floyd: 100\nLloyd: 200\n"
          , "Floyd: 300\nLloyd: 200\n"
          ]

runTelegram :: [Update] -> IO (Seq Text)
runTelegram updates = do
  (telegram, sentRef) <- newTelegram updates
  withSystemTempDirectory "stakes" $ \tmpdir ->
    auction
      telegram
      Bot
        { databaseFile = tmpdir </> "database.sqlite"
        , updateIdFile = tmpdir </> "update_id.txt"
        }
  readIORef sentRef

newTelegram :: [Update] -> IO (Telegram, IORef (Seq Text))
newTelegram updates =
  do
    sentRef <- newIORef mempty
    let sendMessage _ txt = modifyIORef sentRef (|> txt)
    pure (Telegram{getUpdates, sendMessage, putLog}, sentRef)
  where
    getUpdates _ = pure updates
    putLog _ = pure ()

makeUpdate :: Text -> Text -> Update
makeUpdate first_name text = Update{update_id = 59, message}
  where
    chat = Chat{id = 36}
    from = User{first_name, username = Nothing}
    message = Message{chat, text = Just text, from}
