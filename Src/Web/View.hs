{-# LANGUAGE OverloadedStrings #-}

module Web.View ( View(..)
                , Page(..)
                ) where

import           Model.Types
import           Text.Blaze.Html5            ((!))
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

-- |
class View a where
    toBody  :: a -> H.Html
    toTitle :: a -> H.Html

instance View a => View (Maybe a) where
    toBody Nothing   = ""
    toBody (Just a)  = toBody a

    toTitle Nothing  = ""
    toTitle (Just a) = toTitle a

instance View Page where
    toBody Home = do
        H.h1 "Hello World!"
        H.p "This site is currently under construction.  Please come back soon!"

    toTitle Home = H.title "Lofman.co"
