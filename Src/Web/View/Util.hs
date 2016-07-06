{-# LANGUAGE OverloadedStrings #-}

module Web.View.Util where

import           Control.Monad.Cont
import           Data.Monoid
import qualified Data.Text.Lazy              as TL
import qualified Text.Blaze.Html             as H
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

(!:) :: H.Html -> [H.Attribute] -> H.Html
(!:) = foldl (!)

link :: H.Html -> H.AttributeValue -> H.Html
link t url = H.a ! A.href url $ t

iconLink :: H.AttributeValue -> H.AttributeValue -> H.Html
iconLink img = link (H.img ! A.src img)

-- | Simple contstant which contains the URL of the site's logo.
siteIcon :: H.AttributeValue
siteIcon = "img/SiteIcon600.png"
