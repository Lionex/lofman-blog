{-# LANGUAGE TypeOperators #-}

module App
( app
) where

import           API
import           App.Author
import           App.Blog
import           App.Project
import           Data.ByteString.Char8       (pack)
import           Control.Monad.Logger        (runNoLoggingT)
import           Database.Persist
import           Database.Persist.Postgresql
import           Model
import           Network.Wai
import           Servant
import           System.Environment

app :: IO Application
app = serve api <$> server

server :: IO (Server API)
server = do
    -- Create persistent connection pool and start database connection
    connStr <- getEnv "POSTGRESS"
    pool <- runNoLoggingT $ createPostgresqlPool (pack connStr) 10
    runSqlPool (runMigration migrateAll) pool
    -- Run application with pools
    return $ apiServer pool :<|> serveDirectory "static"

apiServer :: ConnectionPool -> Server API'
apiServer pool =
    blogApp pool :<|>
    projectApp pool :<|>
    authorApp pool
