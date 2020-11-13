{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Control.Applicative   ((<|>))
import           Control.Monad         (when)
import           GHC.Generics          (Generic)
import           Options.Generic       (ParseRecord, getRecord)
import           Text.Read             (readMaybe)
import           Text.Shakespeare.Text (stext)
import           Web.Scotty            (ActionM, ScottyM, get, html, param,
                                        scotty)

import           Expr                  (eval)

data Options = Options
  { port    :: Int
  , verbose :: Bool
  }
  deriving (Generic, ParseRecord)

-- when condition action =
--   if condition then
--     action
--   else
--     pure ()

main :: IO ()
main = do
  Options{port, verbose} <- getRecord "A simple web server"
  when verbose $
    putStrLn $ "Starting server at http://localhost:" ++ show port
  server port

server :: Int -> IO ()
server port = scotty port requestMapping

requestMapping :: ScottyM ()
requestMapping =
  get "/" form

form :: ActionM ()
form = do
  pExpr <- param "expr" <|> pure ""
  pX    <- param "x"    <|> pure ""
  let mResult = do
        x    <- readMaybe pX
        expr <- readMaybe pExpr
        pure $ eval expr x
  html
    [stext|
      <form>
        <table>
          <tr><td>expr: <td><input name=expr value="#{pExpr}">
          <tr><td>x:    <td><input name=x value="#{pX}">
          <tr><td>eval: <td>#{show mResult}
        </table>
        <input type=submit>
    |]

-- beam :: ActionM ()
-- beam = do
--   word <- param "word"
--   html $ mconcat ["Scotty, ", word, " me up!"]

--------------------------

-- -- bind
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- getConfig >>= getPort >>= getNetworkData

-- ma >>= f = do a <- ma
--               f a

-- (>>) :: Monad m => m a -> m b -> m b
-- ma >> mb = ma >>= (\_ -> mb)
--   == do
--         ma
--         mb
