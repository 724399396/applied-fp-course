module FirstApp.Types.Error
  ( Error(..)
  ) where

import Database.SQLite.SimpleErrors.Types (SQLiteResponse)

data Error
  = EmptyTopic
  | EmptyComment
  | UnknownRoute
  | SqlError SQLiteResponse
  deriving (Eq, Show)
