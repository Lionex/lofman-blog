{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Handler
( home
, postFromId
, profileFromId
, projectFromId
, error404
) where

import           Clay                          ( Css(..)
                                               , compact
                                               , pretty
                                               , renderWith
                                               )
import           Control.Monad.Cont
import           Control.Monad.Logger          (LoggingT, runStderrLoggingT)
import           Control.Monad.Trans.Resource
import           Data.Monoid
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TL
import           Database.Persist              hiding (get)
import           Database.Persist.Postgresql   hiding (get)
import qualified Database.Persist              as ORM
import           Model.DbTypes
import           Model.Types
import qualified Text.Blaze.Html               as H
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5              as H
import           Web.Spock.Safe
import           Web.View
import           Web.View.Meta
import           Web.View.Template
import           Web.View.Style
import           Web.Heroku

runSql :: (HasSpock m, SpockConn m ~ SqlBackend) =>
          SqlPersistT (LoggingT (ResourceT IO)) a -> m a
runSql action =
    runQuery $ \conn -> runResourceT $ runStderrLoggingT $ runSqlConn action conn

blaze :: (MonadIO m) => H.Html -> ActionCtxT ctx m a
blaze = lazyBytes . renderHtml

clay :: Css -> H.Html
clay = H.style . H.toHtml . (renderWith compact [])

home :: (MonadIO m) => ActionCtxT ctx m ()
home = let sheet = clay $ toStyle Home
        in blaze $ pageTemplate (Home) (Home) >> sheet

postFromId :: (MonadIO m, HasSpock (ActionCtxT ctx m),
              SpockConn (ActionCtxT ctx m) ~ SqlBackend) =>
              BlogPostId -> ActionCtxT ctx m b
postFromId postId = do
    let sheet = clay $ toStyle Home
    postData <- runSql $ ORM.get postId
    blaze $ pageTemplate (postData) (postData) >> sheet

projectFromId :: (MonadIO m, HasSpock (ActionCtxT ctx m),
                 SpockConn (ActionCtxT ctx m) ~ SqlBackend) =>
                 ProjectId -> ActionCtxT ctx m b
projectFromId projectId = do
    let sheet = clay $ toStyle Home
    projectData <- runSql $ ORM.get projectId
    blaze $ pageTemplate (projectData) (projectData) >> sheet

profileFromId :: (MonadIO m, HasSpock (ActionCtxT ctx m),
                 SpockConn (ActionCtxT ctx m) ~ SqlBackend) =>
                 AuthorId -> ActionCtxT ctx m b
profileFromId authorId = do
    let sheet = clay $ toStyle Home
    profileData <- runSql $ ORM.get authorId
    blaze $ pageTemplate (profileData) (profileData) >> sheet

error404 :: (MonadIO m) => ActionCtxT ctx m ()
error404 = let sheet = clay $ toStyle Error404
           in blaze $ pageTemplate (Error404) (Error404) >> sheet
