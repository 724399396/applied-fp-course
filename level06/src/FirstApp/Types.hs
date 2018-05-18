{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module FirstApp.Types
  ( Topic
  , CommentText
  , Comment (..)
  , RqType (..)
  , ContentType (..)
  , Error (..)
  , renderContentType
  , mkTopic
  , getTopic
  , mkCommentText
  , getCommentText
  , fromDbComment
  )where

import           Data.Aeson                 (ToJSON (toEncoding), camelTo2,
                                             defaultOptions, fieldLabelModifier,
                                             genericToEncoding)
import           Data.ByteString            (ByteString)
import           Data.Time                  (UTCTime)
import           FirstApp.DB.Types          (DbComment (DbComment))
import           FirstApp.Types.CommentText
import           FirstApp.Types.Error
import           FirstApp.Types.Topic
import           GHC.Generics

data RqType = AddRq Topic CommentText
  | ViewRq Topic
  | ListRq

data ContentType = PlainText | Json

renderContentType ::
  ContentType -> ByteString
renderContentType PlainText = "text/plain"
renderContentType Json      = "application/json"

newtype CommentId = CommentId Int
  deriving (Show, Eq, ToJSON)

data Comment = Comment
  { commentId    :: CommentId
  , commentTopic :: Topic
  , commentBody  :: CommentText
  , commentTime  :: UTCTime
  }
  deriving (Show, Generic)

fromDbComment :: DbComment -> Either Error Comment
fromDbComment (DbComment dId topic body time) =
  (\t b -> Comment (CommentId dId) t b time) <$> mkTopic topic <*> mkCommentText body

modFieldLabel :: String -> String
modFieldLabel =
  drop 1 . dropWhile (/= '_') . camelTo2 '_'

instance ToJSON Comment where
   toEncoding = genericToEncoding $ defaultOptions {fieldLabelModifier = modFieldLabel }
