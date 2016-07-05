{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Cont
import           Data.Monoid
import qualified Data.Text.Lazy                as TL
import           Model.Types
import           System.Environment
import qualified Text.Blaze.Html               as H
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5              as H
import           Web.Spock.Safe
import           Web.View.Meta
import           Web.View.Template

blaze :: (MonadIO m) => H.Html -> ActionCtxT ctx m a
blaze = lazyBytes . renderHtml

main :: IO ()
main = do
    port <- liftM read (getEnv "PORT")
    runSpock port $ spockT id $ app

app :: SpockCtxT ctx IO ()
app = do
    get root $
        let meta_ = [twitterCard "Art + Technology" "@gwelof" "Gwen Lofman's web page"]
        in blaze $ wrapper "Lofman.co" meta_ (H.h1 "Hello World!")

    get ("post" <//> var) $ \postId ->
        let title = H.toHtml (postId :: TL.Text)
            meta_ = [twitterCard "Yay" "@gwelof" "Gwen Lofman's web page"]
        in blaze $ wrapper title meta_ (H.h1 title)
