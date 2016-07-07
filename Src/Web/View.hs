{-# LANGUAGE OverloadedStrings #-}

module Web.View ( View(..)
                , Page(..)
                ) where

import           Control.Monad               (forM_)
import           Model.Types
import           Text.Blaze.Html5            ((!))
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

-- | Determines how to render data into HTML.
class View a where
    toBody  :: a -> H.Html -- ^ Renders data into content.
    toTitle :: a -> H.Html -- ^ Renders data into page title.

instance View a => View (Maybe a) where
    toBody Nothing   = H.h1 "something went wrong"
    toBody (Just a)  = toBody a

    toTitle Nothing  = H.title "404"
    toTitle (Just a) = toTitle a

instance View a => View [a] where
    toBody xs        = forM_ xs ((H.div ! A.class_ "container") . toBody)

    toTitle []       = toTitle Home
    toTitle (x:xs)   = toTitle x

instance View Page where
    toBody Home      = do
        H.h1 "Hello World!"
        H.p "This site is currently under construction.  Please come back soon!"

    toTitle Home     = H.title "Lofman.co"
