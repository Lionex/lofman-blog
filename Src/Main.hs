{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Cont
import           System.Environment
import qualified Web.Handler        as Handler
import           Web.Spock.Safe

main :: IO ()
main = do
    port <- liftM read (getEnv "PORT")
    runSpock port $ spockT id $ app

app :: SpockCtxT ctx IO ()
app = do
    get root              $ Handler.home
    get ("post" <//> var) $ Handler.postFromId
    get "error"           $ Handler.error404
