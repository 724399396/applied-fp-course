{-# LANGUAGE OverloadedStrings #-}

module FirstApp.DB
  ( initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  , Connection
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

initDB :: FilePath -> IO (Either SQLiteResponse Connection)
initDB dbFile = Sql.runDBAction $ do
  c <- Sql.open dbFile
  Sql.execute_ c "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"
  return c

closeDB :: Connection -> IO ()
closeDB = Sql.close

runDB :: IO a -> AppM a
runDB action = liftToAppM (first SqlError <$> Sql.runDBAction action)

addCommentToTopic ::
     Connection -> Topic -> CommentText -> AppM ()
addCommentToTopic conn t c = do
  time <- liftIO getCurrentTime
  runDB $ Sql.execute conn "INSERT INTO comments(topic,comment,time) values (?,?,?)" ((getTopic t),(getCommentText c),time)

getComments ::
  Connection -> Topic -> AppM [Comment]
getComments conn t = do
  dbComments <- runDB $ Sql.query conn "select * from comments where topic = ?" (Sql.Only (getTopic t))
  liftEither $ traverse fromDbComment dbComments

getTopics ::
  Connection -> AppM [Topic]
getTopics conn = do
  dbComments <- runDB $ Sql.query_ conn "select * from comments"
  liftEither $ traverse (second commentTopic . fromDbComment) dbComments

deleteTopic ::
  Connection -> Topic -> AppM ()
deleteTopic conn t =
  runDB $ Sql.execute conn "delete from comments where topic = ?" (Sql.Only (getTopic t))
