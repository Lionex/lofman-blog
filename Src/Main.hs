{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Clay                                 ( Css(..)
                                                      , compact
                                                      , pretty
                                                      , renderWith
                                                      )
import           Control.Monad.Cont
import           Data.Monoid
import qualified Data.Text.Lazy                       as TL
import qualified Data.Text.Lazy.Encoding              as TL
import           Model.DbTypes
import           Model.Types
import           Network.Wai
import           Network.Wai.Middleware.Static
import           Network.Wai.Middleware.RequestLogger
import           System.Environment
import qualified Text.Blaze.Html                      as H
import           Text.Blaze.Html.Renderer.Utf8        (renderHtml)
import qualified Text.Blaze.Html5                     as H
import           Web.Spock.Safe
import           Web.View
import           Web.View.Meta
import           Web.View.Template
import           Web.View.Style

blaze :: (MonadIO m) => H.Html -> ActionCtxT ctx m a
blaze = lazyBytes . renderHtml

clay :: Css -> H.Html
clay = H.style . H.toHtml . (renderWith compact [])

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
