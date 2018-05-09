{-# LANGUAGE OverloadedStrings #-}
import           FirstApp.Main  (app)

import           Test.Hspec
import           Test.Hspec.Wai

main :: IO ()
main = hspec $ with (pure app) $ do
  describe "POST /{topic}/add" $ do
    it "responds with 200 not implement" $ do
      post "/haskell/add" "intresting" `shouldRespondWith` "not implment add reqesut" {matchStatus = 200}
    it "responds with 400 empty comment" $ do
      post "/haskell/add" "" `shouldRespondWith` "empty comment" {matchStatus = 400}
    it "responds with 400 empty topic" $ do
      post "//add" "intersting" `shouldRespondWith` "empty topic" {matchStatus = 400}

  describe "GET /{topic}/view" $ do
    it "responds with 200 not implement" $ do
      get "/haskell/view" `shouldRespondWith` "not implment view reqesut" {matchStatus = 200}
    it "responds with 400 empty topic" $ do
      get "//view" `shouldRespondWith` "empty topic" {matchStatus = 400}

  describe "GET /list" $ do
    it "responds with 200 not implement" $ do
      get "/list" `shouldRespondWith` "not implment list request" {matchStatus = 200}
