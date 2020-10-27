{-# LANGUAGE OverloadedStrings #-}

import           Web.Scotty (get, html, param, scotty)

main :: IO ()
main =
  scotty 3000 $
    get "/:word" $ do
      beam <- param "word"
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
