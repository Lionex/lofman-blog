{-# LANGUAGE OverloadedStrings #-}

module Web.View.Util where

import           Control.Monad.Cont
import           Data.Monoid
import qualified Data.Text.Lazy              as TL
import qualified Text.Blaze.Html             as H
import           Text.Blaze.Html5            ( Attribute (..)
                                             , AttributeValue (..)
                                             , Html (..) , (!)
                                             )
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

-- | Adds a list of attributes to a self-closing element.
(!:) :: Html -> [Attribute] -> Html
(!:) = foldl (!)

-- | Creates a link with given url and text.
link_ :: Html -> AttributeValue -> Html
link_ t url = H.a ! A.href url $ t

-- | Creates a link in the form of an icon.
--   Partially applies the link function taking the location of the image
--   instead of the text value of the link.
iconLink :: AttributeValue -> AttributeValue -> Html
iconLink img = link_ (H.img ! A.src img)

-- | Simple contstant which contains the URL of the site's logo.
siteIcon :: AttributeValue
siteIcon = "img/SiteIcon600.png"
