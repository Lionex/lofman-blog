{-# LANGUAGE OverloadedStrings #-}

module Web.View ( View(..)
                , Page(..)
                ) where

import           Model.Types
import           Text.Blaze.Html5            ((!))
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

class View a where
    toBody :: a -> H.Html

instance View Page where
    toBody Home = do
        H.h1 "Hello World!"
        H.p "This site is currently under construction.  Please come back soon!"
        H.a ! A.href "https://github.com/Lionex/lofman-blog"
            $ "Follow development here!" 
