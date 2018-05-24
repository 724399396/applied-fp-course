{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Main
    ( runApp,
      app,
      prepareAppReqs
    ) where

import           Control.Monad.IO.Class             (liftIO)
import qualified Data.Aeson                         as A
import           Data.Bifunctor                     (first, bimap)
import           Data.ByteString.Lazy               (ByteString)
import qualified Data.ByteString.Lazy               as LBS
import           Data.ByteString.Lazy.Char8         (pack)
import           Data.Text                          (Text)
import qualified Data.Text.Encoding                 as TE
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)
import           FirstApp.AppM                      (AppM, liftEither, runAppM, Env (..))
import           FirstApp.Conf (parseOptions)
import           FirstApp.DB                        (addCommentToTopic,
                                                     getComments, getTopics,
                                                     initDB)
import           FirstApp.Types                     (FirstAppDB (..), confDBFilePath, getDBFilePath, confPortToWai, ContentType (Json, PlainText),
                                                     Error (EmptyComment, EmptyTopic, SqlError, UnknownRoute),
                                                     RqType (AddRq, ListRq, ViewRq), ConfigError (..),
                                                     mkCommentText, mkTopic,
                                                     renderContentType)
import           Network.HTTP.Types                 (Status, hContentType,
                                                     status200, status400,
                                                     status404, status500)
import           Network.Wai                        (Application, Request,
                                                     Response, pathInfo,
                                                     responseLBS,
                                                     strictRequestBody)
import           Network.Wai.Handler.Warp           (run)
import Control.Monad (join)

mkResponse :: Status -> ContentType -> ByteString -> Response
mkResponse s c b = responseLBS s [(hContentType, renderContentType c)] b

resp200, resp400, resp404, resp500 ::  ContentType -> ByteString -> Response
resp200 = mkResponse status200
resp400 = mkResponse status400
resp404 = mkResponse status404
resp500 = mkResponse status500

mkAddRq :: Text -> ByteString -> Either Error RqType
mkAddRq t b = AddRq <$> mkTopic t <*> mkCommentText (byteStringToText b)
  where byteStringToText = TE.decodeUtf8 . LBS.toStrict

mkViewRq :: Text -> Either Error RqType
mkViewRq t = ViewRq <$> mkTopic t

mkListRq :: Either Error RqType
mkListRq = Right ListRq

mkErrorResponse :: Error -> Response
mkErrorResponse EmptyTopic   = resp400 PlainText "empty topic"
mkErrorResponse EmptyComment = resp400 PlainText "empty comment"
mkErrorResponse UnknownRoute = resp404 PlainText "route not exist"
mkErrorResponse (SqlError e) = resp500 PlainText (pack $ show e)

mkRequest :: Request -> AppM RqType
mkRequest r = liftEither =<< case pathInfo r of
  [t, "add"]  -> liftIO (strictRequestBody r >>= return . mkAddRq t)
  [t, "view"] -> return $ mkViewRq t
  ["list"]    -> return mkListRq
  _           -> return $ Left UnknownRoute

handleRequest :: RqType -> AppM Response
handleRequest (AddRq t b) = (addCommentToTopic t b) *> (pure $ resp200 PlainText "add comment success")
handleRequest (ViewRq t) = (resp200 Json . A.encode) <$> (getComments t)
handleRequest ListRq = (resp200 Json . A.encode) <$> getTopics

app :: Env -> Application
app env r cb = do
  resp <- either mkErrorResponse id <$> runAppM (mkRequest r >>= handleRequest) env
  cb resp

runApp :: IO ()
runApp = do
  cfg <- prepareAppReqs
  case cfg of
    Right e@(Env conf _) -> run (confPortToWai conf) (app e)
    Left e   -> putStrLn $ show e

data StartUpError = DbInitError SQLiteResponse
                    | ConfReadError ConfigError
  deriving Show

prepareAppReqs
  :: IO ( Either StartUpError Env )
prepareAppReqs = do
  conf <- first ConfReadError <$> parseOptions "appconfig.json"
  join <$> traverse (\c -> fmap (bimap DbInitError (\conn -> Env c (FirstAppDB conn))) $ initDB $ getDBFilePath $ confDBFilePath c) conf
