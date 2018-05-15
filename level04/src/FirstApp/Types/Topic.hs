{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Types.Topic
  ( Topic
  , mkTopic
  , getTopic
  )where

import Data.Text (Text)
import qualified Data.Text as T
import FirstApp.Types.Error (Error (EmptyTopic))
import Data.Aeson (ToJSON)

newtype Topic =
  Topic Text
  deriving (Show, ToJSON)

mkTopic :: Text ->
  Either Error Topic
mkTopic t =
  if T.null t
  then Left EmptyTopic
  else Right $ Topic t

getTopic :: Topic -> Text
getTopic (Topic c) = c
