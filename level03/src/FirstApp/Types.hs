{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Types
  ( Topic
  , CommentText
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
import Data.Text (Text)
import qualified Data.Text as T

newtype Topic = Topic Text
  deriving Show

newtype CommentText = CommentText Text
  deriving Show

data RqType = AddRq Topic CommentText
  | ViewRq Topic
  | ListRq

data ContentType = PlainText | Json

data Error = EmptyTopic | EmptyComment | UnknownRoute

renderContentType ::
  ContentType -> ByteString
renderContentType PlainText = "text/plain"
renderContentType Json = "application/json"

mkTopic :: Text ->
  Either Error Topic
mkTopic t =
  if T.null t
  then Left EmptyTopic
  else Right $ Topic t

getTopic :: Topic -> Text
getTopic (Topic c) = c

mkCommentText :: Text ->
  Either Error CommentText
mkCommentText c =
  if T.null c
  then Left EmptyComment
  else Right $ CommentText c

getCommentText :: CommentText -> Text
getCommentText (CommentText c) = c
