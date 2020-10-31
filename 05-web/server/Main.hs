{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Control.Applicative   ((<|>))
import qualified Data.Text             as Text
import           GHC.Generics          (Generic)
import           Options.Generic       (ParseRecord, getRecord)
import           Text.Read             (readEither)
import           Text.Shakespeare.Text (stext)
import           Web.Scotty            (ActionM, get, html, param, scotty)

import           Expr                  (eval)

newtype Options = Options{port :: Int}
  deriving (Generic, ParseRecord)

main :: IO ()
main = do
  Options{port} <- getRecord "A simple web server"
  putStrLn $ "Listening at http://localhost:" ++ show port
  server port

server :: Int -> IO ()
server port =
  scotty port $
    get "/" form

form :: ActionM ()
form = do
  exprText <- param "expr" <|> pure ""
  mx       <- (Just <$> param "x") <|> pure Nothing
  let
    eExpr =
      case exprText of
        "" -> Left ""
        _  -> readEither $ Text.unpack exprText
    evalResult =
      case (eExpr, mx) of
        (Right e, Just x) -> show $ eval e x
        _                 -> ""
  html
    [stext|
      <form>
        <table>
          <tr>
            <td>expr:
            <td><input name=expr value="#{exprText}">
            <td>#{either id show eExpr}
          <tr>
            <td>x:
            <td><input name=x value="#{maybe "" show mx}">
          <tr>
            <td>eval:
            <td>#{evalResult}
        </table>
        <input type=submit>
    |]
