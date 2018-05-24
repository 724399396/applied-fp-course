{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module FirstApp.Types
  ( Topic
  , CommentText
  , Comment (..)
  , RqType (..)
  , ContentType (..)
  , Error (..)
  , PartialConf (..)
  , Port (..)
  , DBFilePath (..)
  , ConfigError (..)
  , Conf (..)
  , FirstAppDB (..)
  , renderContentType
  , mkTopic
  , getTopic
  , mkCommentText
  , getCommentText
  , fromDbComment
  , confPortToWai
  )where

import           Data.Aeson                 (FromJSON, ToJSON (toEncoding),
                                             camelTo2, defaultOptions,
                                             fieldLabelModifier,
                                             genericToEncoding, parseJSON, withObject, (.:?))
import           Data.ByteString            (ByteString)
import           Data.Monoid                (Last (Last))
import           Data.Semigroup             (Semigroup, (<>))
import           Data.Time                  (UTCTime)
import           FirstApp.DB.Types          (DbComment (DbComment))
import           Database.SQLite.Simple             (Connection)
import           FirstApp.Types.CommentText
import           FirstApp.Types.Error
import           FirstApp.Types.Topic
import           GHC.Generics
import           Control.Applicative (liftA2)

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

newtype Port = Port
  { getPort :: Word }
  deriving (Eq, Show, FromJSON)

newtype DBFilePath = DBFilePath
  { getDBFilePath :: FilePath }
  deriving (Eq, Show, FromJSON)

data Conf = Conf
  { confPort       :: Port
  , confDBFilePath :: DBFilePath
  }

confPortToWai :: Conf -> Int
confPortToWai = fromIntegral . getPort . confPort

data ConfigError
  = ConfigError
  | ReadFileError String
  | DecodeConfError String
  | MissingConfig String
  deriving (Show)

data PartialConf = PartialConf
 { pcPort       :: Last Port
 , pcDBFilePath :: Last DBFilePath
 } deriving (Generic, Show)

instance Semigroup PartialConf where
  _a <> _b = PartialConf
    { pcPort = pcPort _a <> pcPort _b
    , pcDBFilePath = pcDBFilePath _a <> pcDBFilePath _b
    }

instance Monoid PartialConf where
  mempty = PartialConf mempty mempty
  mappend = (<>)

instance FromJSON PartialConf where
  parseJSON = withObject "config" $ \v -> liftA2 PartialConf
    (Last <$> v .:? "port")
    (Last <$> v .:? "dbFilePath")

newtype FirstAppDB = FirstAppDB
  { dbConn :: Connection
  }
