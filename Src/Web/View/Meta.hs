{-# LANGUAGE OverloadedStrings #-}

-- The ToMeta typeclass which defines how Metadata for pages is created
module Web.View.Meta ( ToMeta (..)
                     ) where

import           Control.Monad.Cont
import           Data.Monoid
import qualified Data.Text.Lazy              as TL
import           Model.DbTypes
import qualified Text.Blaze.Html             as H
import           Text.Blaze.Html5            ( AttributeValue (..)
                                             , Html (..), (!))
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A
import           Web.View
import           Web.View.Util

-- Internal Helper Functions

-- Metadata for Twitter summary card support.
-- A summary card contains a picture, description, and name for the page.
twitterCard :: AttributeValue -> AttributeValue -> AttributeValue -> Html
twitterCard title user desc = do
    H.meta !: [A.name "twitter:card", A.content "summary"]
    H.meta !: [A.name "twitter:title", A.content title]
    -- The `@name` of the link's owner, used for twitter analytics.
    H.meta !: [A.name "twitter:site", A.content user]
    H.meta !: [A.name "twitter:description", A.content desc]
    H.meta !: [A.name "twitter:image", A.content siteIcon]

-- Metadata for Open Graph API sharing.
-- Open Graph metadata is used by facebook to post nice share links.
openGraph :: AttributeValue -> AttributeValue -> AttributeValue -> Html
openGraph url title desc = do
    H.meta !: [A.name "og:url", A.content url]
    H.meta !: [A.name "og:type", A.content "article"]
    H.meta !: [A.name "og:title", A.content title]
    H.meta !: [A.name "og:description", A.content desc]
    H.meta !: [A.name "og:image", A.content siteIcon]

-- Metadata for Google Site Verification.
-- Enables control of the website through google's search dashboard.
googleVerification :: Html
googleVerification
    = H.meta
    ! A.name "google-site-verification"
    ! A.content "rODBRZX6gBNdpfHZVAB26nyicxYnKE-AuKvIEw6ff-k"

-- The description of the site that appears on search engines.
-- Should be a summary of the content.
siteDescription :: AttributeValue -> Html
siteDescription desc
    = H.meta !: [A.name "description", A.content desc]

-- | Defines how View types become meta-data.
class View a => ToMeta a where
    toMeta :: a -> Html
    toMeta = toTitle

instance ToMeta a => ToMeta (Maybe a) where
    toMeta Nothing  = toMeta Home
    toMeta (Just a) = toMeta a

instance ToMeta a => ToMeta [a] where
    toMeta []     = toMeta Home
    toMeta (x:xs) = toMeta x

instance ToMeta Page where
    toMeta Home = do
        toTitle Home
        siteDescription desc
        googleVerification
        twitterCard title "@GwenLofman" desc
        openGraph "http://Lofman.co" title desc
        where title = "Lofman.co"
              desc  = "Gwen Lofman's blog which recounts her art + technology projects."
