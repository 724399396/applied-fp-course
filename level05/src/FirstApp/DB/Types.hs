module FirstApp.DB.Types where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field)

data DbComment = DbComment
  { commentId :: Int
  , commentTopic :: Text
  , commentBody :: Text
  , commentTime :: UTCTime
  }

instance FromRow DbComment where
  fromRow = DbComment <$> field <*> field <*> field <*> field
