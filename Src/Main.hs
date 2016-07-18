{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           Clay                          ( Css(..)
                                               , compact
                                               , pretty
                                               , renderWith
                                               )
import           Control.Monad.Cont
import           Control.Monad.Logger          (LoggingT, runStderrLoggingT)
import           Control.Monad.Trans.Resource
import qualified Data.ByteString               as B
import           Data.Monoid
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TL
import qualified Data.Text.Encoding            as T
import           Database.Persist              hiding (get)
import           Database.Persist.Postgresql   hiding (get)
import qualified Database.Persist              as ORM
import           Model.DbTypes
import           Model.Types
import           System.Environment
import qualified Text.Blaze.Html               as H
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5              as H
import           Web.Spock.Safe
import           Web.View
import           Web.View.Meta
import           Web.View.Template
import           Web.View.Style
import           Web.Heroku

blaze :: (MonadIO m) => H.Html -> ActionCtxT ctx m a
blaze = lazyBytes . renderHtml

clay :: Css -> H.Html
clay = H.style . H.toHtml . (renderWith compact [])

main :: IO ()
main = do
    -- Get parameters from Heroku
    port <- liftM read (getEnv "PORT")
    params <- dbConnParams
    let conn = B.concat . concat . map (\(a,b) ->
               T.encodeUtf8 a : T.encodeUtf8 b : [])

    -- Create database connection pool
    pool <- runStderrLoggingT $ createPostgresqlPool (conn params) 5
    runStderrLoggingT $ runSqlPool (runMigration migrateAll) pool

    -- Run app with database conneciton pool
    runSpock port $ spock (defaultSpockCfg Nothing (PCPool pool) ()) $ app

-- app :: SpockCtxT ctx IO ()
app = do
    get root $
        let sheet = clay $ toStyle Home
         in blaze $ pageTemplate (Home) (Home) >> sheet

    get ("post" <//> var) $ \postId ->
        let title = H.toHtml (postId :: TL.Text)
            sheet = clay $ toStyle Home
         in blaze $ pageTemplate (Home) (Home) >> sheet

    get "error" $
        let sheet = clay $ toStyle Error404
         in blaze $ pageTemplate (Error404) (Error404) >> sheet
