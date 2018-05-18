{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Main
    ( runApp,
      app,
      prepareAppReqs
    ) where

import           Control.Monad.IO.Class             (liftIO)
import qualified Data.Aeson                         as A
import           Data.Bifunctor                     (first)
import           Data.ByteString.Lazy               (ByteString)
import qualified Data.ByteString.Lazy               as LBS
import           Data.ByteString.Lazy.Char8         (pack)
import           Data.Text                          (Text)
import qualified Data.Text.Encoding                 as TE
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)
import           FirstApp.AppM                      (AppM, liftEither, runAppM)
import           FirstApp.Conf                      (dbFilePath, firstAppConfig)
import           FirstApp.DB                        (FirstAppConf,
                                                     addCommentToTopic,
                                                     getComments, getTopics,
                                                     initDB)
import           FirstApp.Types                     (ContentType (Json, PlainText),
                                                     Error (EmptyComment, EmptyTopic, SqlError, UnknownRoute),
                                                     RqType (AddRq, ListRq, ViewRq),
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

handleRequest :: FirstAppConf -> RqType -> AppM Response
handleRequest conf (AddRq t b) = (addCommentToTopic conf t b) *> (pure $ resp200 PlainText "add comment success")
handleRequest conf (ViewRq t) = (resp200 Json . A.encode) <$> (getComments conf t)
handleRequest conf ListRq = (resp200 Json . A.encode) <$> (getTopics conf)

app :: FirstAppConf -> Application
app db r cb = do
  resp <- either mkErrorResponse id <$> runAppM (mkRequest r >>= handleRequest db)
  cb resp

runApp :: IO ()
runApp = do
  cfg <- prepareAppReqs
  case cfg of
    Right c' -> run 3000 (app c')
    Left e   -> putStrLn $ show e

data StartUpError = DbInitError SQLiteResponse
  deriving Show

prepareAppReqs
  :: IO ( Either StartUpError FirstAppConf )
prepareAppReqs =
  first DbInitError <$> initDB (dbFilePath firstAppConfig)
