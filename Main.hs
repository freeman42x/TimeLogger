{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}



module Main where

-- https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/persistent-db#creating-a-database


import           Control.Concurrent
import           Data.Time.Clock
import           Debug.Trace
import           Graphics.X11
import           Graphics.X11.Xlib.Extras

import           Data.Function
import           Data.List
import           Data.Text                (Text)
import           Database.Persist
import           Database.Persist.Sqlite  (runMigration, runSqlite)
import           Database.Persist.TH      (mkMigrate, mkPersist,
                                           persistLowerCase, share, sqlSettings)

main1 :: IO ()
main1 = do
  d <- openDisplay ""
  loop d

loop :: Display -> IO ()
loop d = do
  time <- getCurrentTime
  (w, _) <- getInputFocus d
  a <- internAtom d "_NET_WM_NAME" False
  p <- getTextProperty d w a
  ps <- wcTextPropertyToTextList d p
  windowAttrs <- getWindowAttributes d w
  print $ show time ++ " Name: " ++ show ps ++ " Width: " ++ show (wa_width windowAttrs)
  threadDelay 1000000
  loop d


share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
LogItem
   title    Text
   begin    UTCTime
   end      UTCTime
   deriving Show
|]

main2 :: IO ()
main2 = runSqlite ":memory:" $ runMigration migrateTables

main :: IO ()
main = do
  runSqlite "db.sqlite" $ runMigration migrateTables
