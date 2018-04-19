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
import           Data.Time.Clock
import           Database.Esqueleto
import           Debug.Trace
import           Graphics.X11
import           Graphics.X11.Xlib.Extras

import           Control.Monad.IO.Class   (liftIO)
import           Data.Function
import           Data.List
import           Data.Text                (Text)
import           Database.Persist
import           Database.Persist.Sqlite  (runMigration, runSqlite)
import           Database.Persist.TH      (mkMigrate, mkPersist,
                                           persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
LogItem
   title    Text
   begin    UTCTime
   end      UTCTime
   deriving Show
|]

main :: IO ()
main =
  runSqlite "db.sqlite" $ do
    runMigration migrateTables
    logItem <- select $ from $ \l -> do
            orderBy [desc (l ^. LogItemId)]
            return (l ^. LogItemId, l ^. LogItemTitle)
    liftIO $ do
      print logItem
      d <- openDisplay ""
      loop d

loop :: Display -> IO ()
loop d = do
  time <- getCurrentTime
  (w, _) <- getInputFocus d
  a <- internAtom d "_NET_WM_NAME" False
  p <- getTextProperty d w a
  ps <- wcTextPropertyToTextList d p
  print $ show time ++ " Name: " ++ show ps
  threadDelay 1000000
  loop d
