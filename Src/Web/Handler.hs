{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Handler
( home
, getId404
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
import           Text.Blaze.Html               (Html (..), toHtml)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5              as H
import           Web.Spock.Safe
import           Web.View
import           Web.View.Meta
import           Web.View.Template
import           Web.View.Style
import           Web.Heroku

-- | Runs queries through persistent to access the databse.
--   Nest database action inside runQuery to gain access to Spock's Database
--   connection pooling.
runSql :: (HasSpock m, SpockConn m ~ SqlBackend) =>
          SqlPersistT (LoggingT (ResourceT IO)) a -> m a
runSql action = runQuery
    -- runQuery needs a function which takes a connection and returns the
    -- result in the proper monad.
    (\conn -> runResourceT $ runStderrLoggingT $ runSqlConn action conn)

{-# INLINE runSql #-}

blaze :: (MonadIO m) => Html -> ActionCtxT ctx m a
blaze = lazyBytes . renderHtml

{-# INLINE blaze #-}

clay :: Css -> Html
clay = H.style . toHtml . (renderWith compact [])

{-# INLINE clay #-}

-- | Searches the database with the id retrieved from the type-safe route.
--   If the id is not found, will automatically use the maybe instance of
--   View and toMeta to serve the error page.
getId404 :: (MonadIO m, PersistEntity val, ToMeta val,
            HasSpock (ActionCtxT ctx m), PersistEntityBackend val ~ SqlBackend,
            SpockConn (ActionCtxT ctx m) ~ SqlBackend) =>
            -- To use getId404, the type of the key must be specified
            -- for the compiler to select the right instance.
            Key val -> ActionCtxT ctx m b
getId404 itemId = do
    item <- runSql $ ORM.get itemId
    blaze $ pageTemplate (item) (item) >> (clay $ toStyle item)


home :: (MonadIO m) => ActionCtxT ctx m ()
home = blaze $ pageTemplate (Home) (Home) >> (clay $ toStyle Home)

-- | Returns the post which maches the ID from the route.
--
postFromId :: (MonadIO m, HasSpock (ActionCtxT ctx m),
              SpockConn (ActionCtxT ctx m) ~ SqlBackend) =>
              BlogPostId -> ActionCtxT ctx m b
postFromId = getId404

projectFromId :: (MonadIO m, HasSpock (ActionCtxT ctx m),
                 SpockConn (ActionCtxT ctx m) ~ SqlBackend) =>
                 ProjectId -> ActionCtxT ctx m b
projectFromId = getId404

profileFromId :: (MonadIO m, HasSpock (ActionCtxT ctx m),
                 SpockConn (ActionCtxT ctx m) ~ SqlBackend) =>
                 AuthorId -> ActionCtxT ctx m b
profileFromId = getId404

error404 :: (MonadIO m) => ActionCtxT ctx m ()
error404 = blaze $ pageTemplate (Error404) (Error404) >> (clay $ toStyle Error404)
