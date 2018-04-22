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
import           Control.Exception.Extensible          (bracket)
import qualified Control.Exception.Extensible          as E
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
import           System.IO.Error                       (catchIOError)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
LogItem
   title    Text
   begin    UTCTime
   end      UTCTime
   deriving Show
|]

runDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runDB = runSqlite "db.sqlite"

main :: IO ()
main =
  runDB $ do
    runMigration migrateTables
    liftIO loop

loop :: IO ()
loop = do
  d <- openDisplay ""
  time <- getCurrentTime
  currentWindowTitle <- getFocusedWindowTitle d
  closeDisplay d
  runDB $ do
    previousLogItem <- select $ from $ \li -> do
            orderBy [desc (li ^. LogItemId)]
            limit 1
            return (li ^. LogItemId, li ^. LogItemTitle)
    liftIO $ print currentWindowTitle
    if not (null previousLogItem)
      && (toStrict (pack currentWindowTitle)
      == unValue (snd $ head previousLogItem)) -- extract / safe Haskell
      then do
        let logItemKey = unValue (fst $ head previousLogItem)
        let logItemTitle = unValue (snd $ head previousLogItem) -- dedup
        update $ \li -> do
           set li [LogItemEnd =. val time]
           where_ (li ^. LogItemId ==. val logItemKey)
      else do
        insert $ LogItem (toStrict $ pack currentWindowTitle) time time -- dedup
        return ()
  threadDelay 1000000
  loop

getFocusedWindowTitle :: Display -> IO String
getFocusedWindowTitle d = do
  (w, _) <- getInputFocus d
  wt <- followTreeUntil d (hasCorrectTitle d) w
  getWindowTitle d wt

getWindowTitle :: Display -> Window -> IO String
getWindowTitle d w = do
  let getProp =
          (internAtom d "_NET_WM_NAME" False >>= getTextProperty d w)
          `catchIOError`
          (\_ -> getTextProperty d w wM_NAME)

      extract prop = do l <- wcTextPropertyToTextList d prop
                        return $ if null l then "" else head l

  bracket getProp (xFree . tp_value) extract
      `catchIOError` \_ -> return ""

getParent :: Display -> Window -> IO Window
getParent dpy w = do
  (_, parent, _) <- queryTree dpy w `catchIOError` const (return (0,0,[]))
  return parent

followTreeUntil :: Display -> (Window -> IO Bool) -> Window -> IO Window
followTreeUntil dpy cond = go
  where go w = do
          match <- cond w
          if match
            then return w
            else
              do p <- getParent dpy w
                 if p == 0 then return w
                           else go p

hasCorrectTitle :: Display -> Window -> IO Bool
hasCorrectTitle d w = do
  title <- getWindowTitle d w
  return $ title /= "" && title /= "FocusProxy"

-- TODO
-- Meld has empty title. Fallback to process data (executable name)?
