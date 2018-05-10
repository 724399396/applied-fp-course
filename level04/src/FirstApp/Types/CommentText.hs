{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Types.CommentText
  ( CommentText
  , mkCommentText
  , getCommentText
  ) where

import FirstApp.Types.Error (Error (EmptyComment))

import Data.Text (Text)
import qualified Data.Text as T

newtype CommentText = CommentText Text
  deriving Show

mkCommentText :: Text ->
  Either Error CommentText
mkCommentText c =
  if T.null c
  then Left EmptyComment
  else Right $ CommentText c

getCommentText :: CommentText -> Text
getCommentText (CommentText c) = c
