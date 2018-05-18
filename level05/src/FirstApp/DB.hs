{-# LANGUAGE OverloadedStrings #-}

module FirstApp.DB
  ( initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  , FirstAppConf
  ) where

import Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.SimpleErrors as Sql
import FirstApp.AppM
import FirstApp.Types (fromDbComment, Comment (commentTopic), CommentText, Error (SqlError), Topic, getCommentText, getTopic)
import Data.Bifunctor (first, second)
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class (liftIO)
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

data FirstAppConf = FirstAppConf
  { conn :: Connection
  }

initDB :: FilePath -> IO (Either SQLiteResponse FirstAppConf)
initDB dbFile = Sql.runDBAction $ do
  c <- Sql.open dbFile
  Sql.execute_ c "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"
  return (FirstAppConf c)

closeDB :: FirstAppConf -> IO ()
closeDB = Sql.close . conn

runDB :: IO a -> AppM a
runDB action = liftToAppM (first SqlError <$> Sql.runDBAction action)

addCommentToTopic ::
     FirstAppConf -> Topic -> CommentText -> AppM ()
addCommentToTopic conf t c = do
  time <- liftIO getCurrentTime
  runDB $ Sql.execute (conn conf) "INSERT INTO comments(topic,comment,time) values (?,?,?)" ((getTopic t),(getCommentText c),time)

getComments ::
  FirstAppConf -> Topic -> AppM [Comment]
getComments conf t = do
  dbComments <- runDB $ Sql.query (conn conf) "select * from comments where topic = ?" (Sql.Only (getTopic t))
  liftEither $ traverse fromDbComment dbComments

getTopics ::
  FirstAppConf -> AppM [Topic]
getTopics conf = do
  dbComments <- runDB $ Sql.query_ (conn conf) "select * from comments"
  liftEither $ traverse (second commentTopic . fromDbComment) dbComments

deleteTopic ::
  FirstAppConf -> Topic -> AppM ()
deleteTopic conf t =
  runDB $ Sql.execute (conn conf) "delete from comments where topic = ?" (Sql.Only (getTopic t))
