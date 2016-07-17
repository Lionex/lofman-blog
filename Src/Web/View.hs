{-# LANGUAGE OverloadedStrings #-}

module Web.View
( View(..)
) where

import           Control.Monad               (forM_)
import           Clay                        (Css (..), (#), (?))
import qualified Clay                        as C
import           Model.Types
import           Model.DbTypes
import           Text.Blaze.Html5            (Html (..), (!))
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A
import qualified Web.View.Style              as S
import           Web.View.Util


-- | Determines how to render data into HTML.
class View a where
    toBody  :: a -> Html -- ^ Renders data into content.

    toTitle :: a -> Html -- ^ Renders data into page title.
    toTitle _ = "Lofman.co"

    toStyle :: a -> Css
    toStyle _ = S.baseStyle

instance View a => View (Maybe a) where
    toBody Nothing   = toBody Error404
    toBody (Just a)  = toBody a

    toTitle Nothing  = toBody Error404
    toTitle (Just a) = toTitle a

instance View a => View [a] where
    toBody xs        = H.div ! A.class_ "container" $ forM_ xs (toBody)

    toTitle []       = toTitle Home
    toTitle (x:xs)   = toTitle x

instance View Page where
    toBody Home      = do
        H.h1 "Hello World!"
        H.p  "This site is currently under construction.  Please come back soon!"
    toBody Error404  = do
        H.h1 "Error 404"
        H.p $ do "Something went wrong, go "
                 link_ "home" "/"

    toTitle Home     = H.title "Lofman.co"
    toTitle Error404 = H.title "Lofman.co: Error, not found."

    toStyle Home     = S.baseStyle >> S.mastHead
    toStyle Error404 = S.baseStyle >> S.noMastHead
