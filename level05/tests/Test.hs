{-# LANGUAGE OverloadedStrings #-}
import           FirstApp.Main  (app, prepareAppReqs)

import Control.Monad (join)
import qualified System.Exit as Exit
import qualified FirstApp.DB as DB
import qualified FirstApp.Types as Types
import           Test.Hspec
import           Test.Hspec.Wai

main :: IO ()
main = do
  let dieWith m = print m >> Exit.exitFailure
  conf <- prepareAppReqs
  case conf of
    Left e -> dieWith e
    Right c -> do
      let app' = pure (app c)
          flushTopic = (join <$> traverse (DB.deleteTopic c) (Types.mkTopic "haskell")) >>= either dieWith return
      hspec $ with (flushTopic >> app') $ do
        describe "POST /{topic}/add" $ do
          it "responds with 200" $ do
            post "/haskell/add" "intresting" `shouldRespondWith` "add comment success" {matchStatus = 200}
          it "responds with 400 empty comment" $ do
            post "/haskell/add" "" `shouldRespondWith` "empty comment" {matchStatus = 400}
          it "responds with 400 empty topic" $ do
            post "//add" "intersting" `shouldRespondWith` "empty topic" {matchStatus = 400}

        describe "GET /{topic}/view" $ do
          it "responds with 200" $ do
            post "/haskell/add" "intresting" `shouldRespondWith` "add comment success" {matchStatus = 200}
            get "/haskell/view" `shouldRespondWith` 200
          it "responds with 400 empty topic" $ do
            get "//view" `shouldRespondWith` "empty topic" {matchStatus = 400}

        describe "GET /list" $ do
          it "responds with 200" $ do
            post "/haskell/add" "intresting" `shouldRespondWith` "add comment success" {matchStatus = 200}
            get "/list" `shouldRespondWith` 200
