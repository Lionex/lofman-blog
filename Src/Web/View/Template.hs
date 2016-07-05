{-# LANGUAGE OverloadedStrings #-}

module Web.View.Template where

import           Control.Monad.Cont
import           Data.Monoid
import qualified Data.Text.Lazy              as TL
import qualified Text.Blaze.Html             as H
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A
import           Web.View.Meta
import           Web.View.Util

wrapper :: H.Html -> H.Html -> H.Html -> H.Html
wrapper title meta_ body = H.docTypeHtml $ do
    H.head $ do
        H.title title
        H.meta ! A.charset "utf-8"
        meta_
    H.body $ do
        siteNav nav
        H.main ! A.class_ "content" $ do body
        footer [H.a "Facebook"] nav
    where nav = [H.a "home" ! A.href "/"]

siteNav :: [H.Html] -> H.Html
siteNav nav = H.header ! A.class_ "siteHead" $ do
    H.nav $ do
        H.span $ H.img ! A.src siteIcon
        H.ul $ forM_ nav (H.li)

footer :: [H.Html] -> [H.Html] -> H.Html
footer social nav = H.footer ! A.class_ "site" $ do
    H.article ! A.class_ "profile" $ do
        H.h2 "Gwen Lofman"
        H.ul ! A.class_ "social" $ forM_ social (H.li)

    H.nav $ do
        H.ul ! A.class_ "nav" $ forM_ nav (H.li)
