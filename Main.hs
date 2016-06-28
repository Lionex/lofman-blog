{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Cont
import           Data.Monoid
import qualified Data.Text.Lazy                as TL
import qualified Text.Blaze.Html               as H
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5              as H
import           Web.Spock.Safe


helloSpock :: SpockAction db session state ()
helloSpock = do html "Hello, <em> Spock </em>"


app :: SpockM database session state ()
app = do get "/" helloSpock


main :: IO ()
main = runSpock 8080 $ spockT id $
    do get root $
           text "Hello World!"
       get ("post" <//> var) $ \postId ->
           text ("Not implemented, unable to get " <> postId <> " from database.")
