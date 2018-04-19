{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           Control.Concurrent
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource.Internal
import           Data.Function
import           Data.List                             (null)
import           Data.Text                             (Text)
import           Data.Text.Lazy                        (fromStrict, pack,
                                                        toStrict)
import           Data.Time.Clock
import           Database.Esqueleto
import           Database.Persist                      (insert)
import           Database.Persist.Sqlite               (runMigration, runSqlite)
import           Database.Persist.TH                   (mkMigrate, mkPersist,
                                                        persistLowerCase, share,
                                                        sqlSettings)
import           Debug.Trace
import           Graphics.X11
import           Graphics.X11.Xlib.Extras

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
LogItem
   title    Text
   begin    UTCTime
   end      UTCTime
   deriving Show
|]

-- data Item = Item { id    :: Int
--                  , title :: String
--                  } deriving (Show)

runDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runDB = runSqlite "db.sqlite"

main :: IO ()
main =
  runDB $ do
    runMigration migrateTables

    liftIO $ do
      d <- openDisplay ""
      loop d

loop :: Display -> IO ()
loop d = do
  time <- getCurrentTime
  (w, _) <- getInputFocus d
  a <- internAtom d "_NET_WM_NAME" False
  p <- getTextProperty d w a
  currentWindowTitles <- wcTextPropertyToTextList d p
  let currentWindowTitle = concat currentWindowTitles
  runDB $ do
    previousLogItem <- select $ from $ \li -> do
            orderBy [desc (li ^. LogItemId)]
            limit 1
            return (li ^. LogItemId, li ^. LogItemTitle)
    if not (null previousLogItem)
      && (toStrict (pack currentWindowTitle)
      == unValue (snd $ head previousLogItem)) -- extract / safe Haskell
      then do
        let logItemKey = unValue (fst $ head previousLogItem)
        let logItemTitle = unValue (snd $ head previousLogItem) -- dedup
        update $ \li -> do
           set li [LogItemTitle =. val logItemTitle]
           where_ (li ^. LogItemId ==. val logItemKey)
      else do
        insert $ LogItem (toStrict $ pack currentWindowTitle) time time -- dedup
        return ()
  threadDelay 1000000
  loop d
