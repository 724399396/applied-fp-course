{-# LANGUAGE OverloadedStrings #-}

module FirstApp.DB where

import Database.SQLite.Simple (query, Connection, execute)
import Database.SQLite.SimpleErrors (runDBAction)
import FirstApp.Types (Comment, CommentText, Error (SqlError), Topic, getCommentText, getTopic)
import Data.Bifunctor (first)
import Data.Time (getCurrentTime)

data FirstAppConf = FirstAppConf
  { conn :: Connection
  }

addCommentToTopic ::
     FirstAppConf -> Topic -> CommentText -> IO (Either Error ())
addCommentToTopic conf t c = do
  time <- getCurrentTime
  first SqlError <$> (runDBAction $ execute (conn conf) "INSERT INTO comments(topic,comment,time) values (?,?,?)" ((getTopic t),(getCommentText c),time))

getComments ::
  FirstAppConf -> Topic -> IO (Either Error [Comment])
getComments conf t =
  first SqlError <$> (runDBAction $ query (conn conf) "select * from comments where topic = ?" (Only (getTopic t)))
