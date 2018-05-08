{-# LANGUAGE OverloadedStrings #-}

module FirstApp.Main
    ( runApp
    ) where

import Network.Wai (strictRequestBody, pathInfo, Application, responseLBS, Response, Request)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (Status, hContentType, status200, status400, status404)
import Data.ByteString.Lazy (ByteString)
import FirstApp.Types (mkCommentText, ContentType (PlainText), renderContentType, mkTopic, RqType (AddRq, ViewRq, ListRq), Error (EmptyTopic, EmptyComment, UnknownRoute))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS

mkResponse :: Status -> ContentType -> ByteString -> Response
mkResponse s c b = responseLBS s [(hContentType, renderContentType c)] b

resp200, resp400, resp404 ::  ContentType -> ByteString -> Response
resp200 = mkResponse status200
resp400 = mkResponse status400
resp404 = mkResponse status404

mkAddRq :: Text -> ByteString -> Either Error RqType
mkAddRq t b = AddRq <$> mkTopic t <*> mkCommentText (byteStringToText b)
  where byteStringToText = TE.decodeUtf8 . LBS.toStrict

mkViewRq :: Text -> Either Error RqType
mkViewRq t = ViewRq <$> mkTopic t

mkListRq :: Either Error RqType
mkListRq = Right ListRq

mkErrorResponse :: Error -> Response
mkErrorResponse EmptyTopic = resp400 PlainText "empty topic"
mkErrorResponse EmptyComment = resp400 PlainText "empty comment"
mkErrorResponse UnknownRoute = resp404 PlainText "route not exist"

mkRequest :: Request -> IO (Either Error RqType)
mkRequest r = case pathInfo r of
  [t, "add"] -> strictRequestBody r >>= return . mkAddRq t
  [t, "view"] -> return $ mkViewRq t
  ["list"] -> return mkListRq
  _ -> return $ Left UnknownRoute

handleRequest :: RqType
  -> Either Error Response
handleRequest (AddRq _ _) = return $ resp200 PlainText "not implment add reqesut"
handleRequest (ViewRq _) = return $ resp200 PlainText "not implment view reqesut"
handleRequest ListRq = return $ resp200 PlainText "not implment list request"

app :: Application
app r cb = do r' <- mkRequest r
              cb $ either mkErrorResponse id (r' >>= handleRequest)

runApp :: IO ()
runApp = run 3000 app
