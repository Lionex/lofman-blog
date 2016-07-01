{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Cont
import           Data.Monoid
import qualified Data.Text.Lazy                as TL
import           System.Environment
import qualified Text.Blaze.Html               as H
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5              as H
import           Web.Spock.Safe




helloSpock :: SpockAction db session state ()
helloSpock = do html "Hello, <em> Spock </em>"


app :: SpockM database session state ()
app = do get "/" helloSpock


main :: IO ()
main = do
    -- Parse the port from Heroku $PORT variable
    port <- liftM read (getEnv "PORT")

    -- Run the web app
    runSpock port $ spockT id $
        do get root $ text "Hello World!"
           get ("post" <//> var) $ \postId -> text ("Hello" <> postId)
