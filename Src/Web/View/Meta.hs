{-# LANGUAGE OverloadedStrings #-}

-- The ToMeta typeclass which defines how Metadata for pages is created
module Web.View.Meta ( ToMeta(..)
                     ) where

import           Control.Monad.Cont
import           Data.Monoid
import qualified Data.Text.Lazy              as TL
import           Model.Types
import qualified Text.Blaze.Html             as H
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A
import           Web.View
import           Web.View.Util

-- Internal Helper Functions

-- Metadata for Twitter summary card support.
-- A summary card contains a picture, description, and name for the page.
twitterCard :: H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.Html
twitterCard title user desc = do
    H.meta !: [A.name "twitter:card", A.content "summary"]
    H.meta !: [A.name "twitter:title", A.content title]
    -- The `@name` of the link's owner, used for twitter analytics.
    H.meta !: [A.name "twitter:site", A.content user]
    H.meta !: [A.name "twitter:description", A.content desc]
    H.meta !: [A.name "twitter:image", A.content siteIcon]

-- Metadata for Open Graph API sharing.
-- Open Graph metadata is used by facebook to post nice share links.
openGraph :: H.AttributeValue -> H.AttributeValue -> H.AttributeValue -> H.Html
openGraph url title desc = do
    H.meta !: [A.name "og:url", A.content url]
    H.meta !: [A.name "og:type", A.content "article"]
    H.meta !: [A.name "og:title", A.content title]
    H.meta !: [A.name "og:description", A.content desc]
    H.meta !: [A.name "og:image", A.content siteIcon]

-- Metadata for Google Site Verification.
-- Enables control of the website through google's search dashboard.
googleVerification :: H.Html
googleVerification
    = H.meta
    ! A.name "google-site-verification"
    ! A.content "rODBRZX6gBNdpfHZVAB26nyicxYnKE-AuKvIEw6ff-k"

-- The description of the site that appears on search engines.
-- Should be a summary of the content.
siteDescription :: H.AttributeValue -> H.Html
siteDescription desc
    = H.meta !: [A.name "description", A.content desc]

-- | Defines how View types become meta-data.
class View a => ToMeta a where
    toMeta :: a -> H.Html

instance ToMeta Page where
    toMeta Home = do
        siteDescription desc
        googleVerification
        twitterCard title "@GwenLofman" desc
        openGraph "http://Lofman.co" title desc
        where title = "Lofman.co"
              desc  = "Gwen Lofman's blog which recounts her art + technology projects."
