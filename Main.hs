{-# LANGUAGE OverloadedStrings #-}

module Main where
import           Control.Monad.Cont
import           Data.Monoid
import qualified Data.Text.Lazy                as TL
import           System.Environment
import qualified Text.Blaze.Html               as H
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5              as H
import           Web.Spock.Safe
import           Web.Template


blaze :: (MonadIO m) => H.Html -> ActionCtxT ctx m a
blaze = lazyBytes . renderHtml



--The Main Function
-------------------

main :: IO ()
main = do
  port <- liftM read (getEnv "PORT")
  runSpock port $ spockT id $
    do get root $ blaze $ wrapper "Lofman.co" (H.meta) (H.h1 "Hello World!")
       get ("post" <//> var) $ \postId ->
         text ("Hello " <> postId)
