{-# LANGUAGE OverloadedStrings #-}

module Web.View.Template where

import           Control.Monad.Cont
import           Data.Monoid
import qualified Data.Text.Lazy              as TL
import qualified Text.Blaze.Html             as H
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A
import           Web.View.Util

wrapper :: H.Html -> H.Html -> H.Html
wrapper meta body = H.docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "utf-8"
        meta
    H.body $ do
        siteNav []
        H.main ! A.class_ "content" $ body
        footer [H.a "Facebook" ! A.href "https://www.facebook.com/gwen.lofman"] []

siteNav :: [H.Html] -> H.Html
siteNav nav = H.header ! A.class_ "siteHead" $ do
    H.nav ! A.class_ "crossfade" $ do
        H.ul ! A.class_ "nav" $ do
            H.span $ H.a ! A.href "/" $ H.img ! A.src siteIcon
            forM_ nav (H.li ! A.class_ "nav-button")

footer :: [H.Html] -> [H.Html] -> H.Html
footer social nav = H.footer ! A.class_ "site" $ do
    H.aside ! A.class_ "profile" $ do
        H.h2 "Gwen Lofman"
        H.ul ! A.class_ "social" $ forM_ social (H.li)

    H.nav  $ do
        H.ul ! A.class_ "nav nav-footer" $ forM_ nav (H.li)
