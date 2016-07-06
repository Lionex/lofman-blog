{-# LANGUAGE OverloadedStrings #-}

module Web.View.Meta where

import           Control.Monad.Cont
import           Data.Monoid
import qualified Data.Text.Lazy              as TL
import qualified Text.Blaze.Html             as H
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A
import           Web.View.Util


twitterCard :: H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.Html
twitterCard title user desc = do
    H.meta !: [A.name "twitter:card", A.content "summary"]
    H.meta !: [A.name "twitter:title", A.content title]
    H.meta !: [A.name "twitter:site", A.content user]
    H.meta !: [A.name "twitter:description", A.content desc]
    H.meta !: [A.name "twitter:image", A.content siteIcon]
