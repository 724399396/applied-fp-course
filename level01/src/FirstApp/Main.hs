{-# LANGUAGE OverloadedStrings #-}

module FirstApp.Main
    ( runApp
    ) where

import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)

app :: Application
app _ cb = cb (responseLBS status200 [] "Hello, World!")

runApp :: IO ()
runApp = run 3000 app
