{-# LANGUAGE OverloadedStrings #-}

module Web.View.Template (pageTemplate) where

import           Control.Monad.Cont
import           Data.Monoid
import qualified Data.Text.Lazy              as TL
import qualified Text.Blaze.Html             as H
import           Text.Blaze.Html5            (Html (..), (!))
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A
import           Web.View
import           Web.View.Meta
import           Web.View.Util

pageTemplate :: (ToMeta a, View b) => a -> b -> Html
pageTemplate meta body = H.docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "utf-8"
        toMeta meta
    H.body $ do
        H.header ! A.class_ "mastHead" $ ""
        siteNav nav
        H.main $ toBody body
        footer [link_ "Facebook" "https://www.facebook.com/gwen.lofman"] nav
    where nav = [ link_ "Projects" "/projects"
                , link_ "Posts" "/posts"
                ]

siteNav :: [Html] -> Html
siteNav nav = H.nav ! A.class_ "crossfade site-nav" $ do
    H.ul ! A.class_ "nav" $ do
        H.span $ H.a ! A.href "/" $ H.img ! A.src siteIcon
        let wrap = H.li ! A.class_ "nav-button"
         in forM_ nav wrap

footer :: [Html] -> [Html] -> Html
footer social nav = H.footer ! A.class_ "site" $ do
    H.aside ! A.class_ "profile" $ do
        H.h2 "Gwen Lofman"
        let wrap = H.li ! A.class_ "nav-button social-nav-button"
         in H.ul ! A.class_ "social" $ forM_ social wrap

    H.nav ! A.class_ "footer-nav" $ do
        let wrap = H.li ! A.class_ "nav-button footer-nav-button"
         in H.ul ! A.class_ "nav" $ forM_ nav wrap
