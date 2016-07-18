{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Cont
import           Control.Monad.Logger          (LoggingT, runStderrLoggingT)
import qualified Data.Text.Encoding            as T
import qualified Data.ByteString               as B
import           Database.Persist              hiding (get)
import           Database.Persist.Postgresql   hiding (get)
import           System.Environment
import           Model.DbTypes
import           Network.Wai
import           Network.Wai.Middleware.Static
import           Network.Wai.Middleware.RequestLogger
import qualified Web.Handler                          as Handler
import           Web.Spock.Safe
import           Web.Heroku

main :: IO ()
main = do
    -- Get parameters from Heroku
    port <- liftM read (getEnv "PORT")
    params <- dbConnParams
    let conn = B.concat . concat . map (\(a,b) ->
               T.encodeUtf8 a : "=" : T.encodeUtf8 b : " " : [])

    -- Create database connection pool
    pool <- runStderrLoggingT $ createPostgresqlPool (conn params) 5
    runStderrLoggingT $ runSqlPool (runMigration migrateAll) pool

    -- Run app with database conneciton pool
    runSpock port $ spock (defaultSpockCfg Nothing (PCPool pool) ())
                  $ appMiddleware >> app

appMiddleware :: SpockCtxT () (WebStateM SqlBackend (Maybe a) ()) ()
appMiddleware = do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "static")

app :: SpockM SqlBackend (Maybe a0) () ()
app = do
    get root                 $ Handler.home
    get ("post" <//> var)    $ Handler.postFromId
    get ("project" <//> var) $ Handler.projectFromId
    get ("profile" <//> var) $ Handler.profileFromId
    get "error"              $ Handler.error404
