module Web.Handler
( home
, postFromId
, error404
) where

import           Clay                          ( Css(..)
                                               , compact
                                               , pretty
                                               , renderWith
                                               )
import           Control.Monad.Cont
import           Data.Monoid
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TL
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

blaze :: (MonadIO m) => H.Html -> ActionCtxT ctx m a
blaze = lazyBytes . renderHtml

clay :: Css -> H.Html
clay = H.style . H.toHtml . (renderWith compact [])

home :: (MonadIO m) => ActionCtxT ctx m ()
home = let sheet = clay $ toStyle Home
        in blaze $ pageTemplate (Home) (Home) >> sheet

postFromId :: (MonadIO m) => TL.Text -> ActionCtxT ctx m ()
postFromId postId = let sheet = clay $ toStyle Home
                     in blaze $ pageTemplate (Home) (Home) >> sheet

error404 :: (MonadIO m) => ActionCtxT ctx m ()
error404 = let sheet = clay $ toStyle Error404
           in blaze $ pageTemplate (Error404) (Error404) >> sheet
