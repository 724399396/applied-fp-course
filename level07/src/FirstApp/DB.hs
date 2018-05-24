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
import FirstApp.Types (fromDbComment, Comment (commentTopic), CommentText, Error (SqlError), Topic, getCommentText, getTopic, dbConn)
import Data.Bifunctor (first, second)
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class (liftIO)
import Database.SQLite.SimpleErrors.Types (SQLiteResponse)
import Control.Monad.Reader (ask)

initDB :: FilePath -> IO (Either SQLiteResponse Connection)
initDB dbFile = Sql.runDBAction $ do
  c <- Sql.open dbFile
  Sql.execute_ c "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"
  return c

closeDB :: Connection -> IO ()
closeDB = Sql.close

runDB :: IO a -> AppM a
runDB action = liftToAppM (first SqlError <$> Sql.runDBAction action)

addCommentToTopic :: Topic -> CommentText -> AppM ()
addCommentToTopic t c = do
  conn <- (dbConn . envDB) <$> ask
  time <- liftIO getCurrentTime
  runDB $ Sql.execute conn "INSERT INTO comments(topic,comment,time) values (?,?,?)" ((getTopic t),(getCommentText c),time)

getComments :: Topic -> AppM [Comment]
getComments t = do
  conn <- (dbConn . envDB) <$> ask
  dbComments <- runDB $ Sql.query conn "select * from comments where topic = ?" (Sql.Only (getTopic t))
  liftEither $ traverse fromDbComment dbComments

getTopics :: AppM [Topic]
getTopics = do
  conn <- (dbConn . envDB) <$> ask
  dbComments <- runDB $ Sql.query_ conn "select * from comments"
  liftEither $ traverse (second commentTopic . fromDbComment) dbComments

deleteTopic :: Topic -> AppM ()
deleteTopic t = do
  conn <- (dbConn . envDB) <$> ask
  runDB $ Sql.execute conn "delete from comments where topic = ?" (Sql.Only (getTopic t))
