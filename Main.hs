{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import           Control.Concurrent
import           Control.Exception.Extensible          (bracket)
import qualified Control.Exception.Extensible          as E
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource.Internal
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Function
import           Data.List                             (null)
import           Data.Proxy
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
import           GHC.Generics                          (Generic)
import           Graphics.X11
import           Graphics.X11.Xlib.Extras
import qualified Network.Wai                           as Wai
import qualified Network.Wai.Handler.Warp              as Wai
import qualified Network.Wai.Middleware.Gzip           as Wai
import qualified Network.Wai.Middleware.RequestLogger  as Wai
import           Servant                               ((:<|>) (..), (:>), Get)
import qualified Servant
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
main = do
  _ <- forkIO $ Wai.run 3003 $ Wai.logStdout $ compress app
  runDB $ do
    runMigration migrateTables
    liftIO loop

compress :: Wai.Middleware
compress = Wai.gzip Wai.def { Wai.gzipFiles = Wai.GzipCompress }

app :: Wai.Application
app = Servant.serve userAPI server

userAPI :: Proxy UserAPI
userAPI = Proxy

type UserAPI = "users" :> Get '[Servant.JSON] [User]
          :<|> "static" :> Servant.Raw

static :: Servant.Server StaticAPI
static = Servant.serveDirectoryFileServer "static"

server :: Servant.Server UserAPI
server = return users
          :<|> static

type ServerAPI = StaticAPI

type StaticAPI = "static" :> Servant.Raw

data User = User
  { name  :: String
  , age   :: Int
  , email :: String
  } deriving (Eq, Show, Generic)

instance ToJSON User

users :: [User]
users =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk"
  , User "Albert Einstein" 136 "ae@mc2.org"
  ]

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
