{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Types
  ( Topic
  , CommentText
  , Comment
  , RqType (..)
  , ContentType (..)
  , Error (..)
  , renderContentType
  , mkTopic
  , getTopic
  , mkCommentText
  , getCommentText
  )where

import Data.ByteString (ByteString)
import FirstApp.Types.CommentText
import FirstApp.Types.Topic
import FirstApp.Types.Error
import Data.Time (UTCTime)
import FirstApp.DB.Types (DbComment (DbComment))

data RqType = AddRq Topic CommentText
  | ViewRq Topic
  | ListRq

data ContentType = PlainText | Json

renderContentType ::
  ContentType -> ByteString
renderContentType PlainText = "text/plain"
renderContentType Json = "application/json"

newtype CommentId = CommentId Int
  deriving (Show, Eq)

data Comment = Comment
  { commentId :: CommentId
  , commentTopic :: Topic
  , commentBody :: CommentText
  , commentTime :: UTCTime
  }
  deriving (Show)

fromDbComment :: DbComment -> Either Error Comment
fromDbComment (DbComment dId topic body time) =
  (\t b -> Comment (CommentId dId) t b time) <$> mkTopic topic <*> mkCommentText body
