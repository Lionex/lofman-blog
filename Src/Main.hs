{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Cont
import           System.Environment
import           Network.Wai
import           Network.Wai.Middleware.Static
import           Network.Wai.Middleware.RequestLogger
import qualified Web.Handler                          as Handler
import           Web.Spock.Safe

main :: IO ()
main = do
    port <- liftM read (getEnv "PORT")
    runSpock port $ spockT id $ appMiddleware app

appMiddleware :: SpockT IO ()
appMiddleware = do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "static")

app :: SpockCtxT ctx IO ()
app = do
    get root              $ Handler.home
    get ("post" <//> var) $ Handler.postFromId
    get "error"           $ Handler.error404
