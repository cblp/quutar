{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Database (EntityField (StakeValue), Stake (..), StakeId, runDb) where

import           Control.Monad.Logger       (NoLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Int                   (Int64)
import           Data.Text                  (Text)
import           Database.Persist.Sqlite    (EntityField, SqlBackend,
                                             runMigration, runSqlite)
import           Database.Persist.TH        (mkMigrate, mkPersist,
                                             persistLowerCase, share,
                                             sqlSettings)
import           UnliftIO.Resource          (ResourceT)

$(share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Stake
      username  Text
      value     Int64
      UniqueUsername username
      deriving Show
  |])

type DatabaseAction = ReaderT SqlBackend (NoLoggingT (ResourceT IO))

runDb :: Text -> DatabaseAction result -> IO result
runDb file action =
  runSqlite file $ do
    runMigration migrateAll
    action
