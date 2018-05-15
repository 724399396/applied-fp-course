{-# LANGUAGE OverloadedStrings #-}

module FirstApp.DB
  ( initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , FirstAppConf
  ) where

import Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.SimpleErrors as Sql
import FirstApp.Types (fromDbComment, Comment (commentTopic), CommentText, Error (SqlError), Topic, getCommentText, getTopic)
import Data.Bifunctor (first, second)
import Data.Time (getCurrentTime)

data FirstAppConf = FirstAppConf
  { conn :: Connection
  }

initDB :: FilePath -> IO (Either Error FirstAppConf)
initDB dbFile = runDB $ do
  c <- Sql.open dbFile
  Sql.execute_ c "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"
  return (FirstAppConf c)

closeDB :: FirstAppConf -> IO ()
closeDB = Sql.close . conn

runDB :: IO a -> IO (Either Error a)
runDB action = first SqlError <$> Sql.runDBAction action

addCommentToTopic ::
     FirstAppConf -> Topic -> CommentText -> IO (Either Error ())
addCommentToTopic conf t c = do
  time <- getCurrentTime
  runDB $ Sql.execute (conn conf) "INSERT INTO comments(topic,comment,time) values (?,?,?)" ((getTopic t),(getCommentText c),time)

getComments ::
  FirstAppConf -> Topic -> IO (Either Error [Comment])
getComments conf t =
  (traverse fromDbComment =<<) <$> (runDB $ Sql.query (conn conf) "select * from comments where topic = ?" (Sql.Only (getTopic t)))

getTopics ::
  FirstAppConf -> IO (Either Error [Topic])
getTopics conf =
  (traverse (second commentTopic . fromDbComment) =<<) <$> (runDB $ Sql.query_ (conn conf) "select * from comments")
