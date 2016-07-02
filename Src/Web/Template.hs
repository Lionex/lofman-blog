{-# LANGUAGE OverloadedStrings #-}

module Web.Template where

import           Control.Monad.Cont
import           Data.Monoid
import qualified Data.Text.Lazy                as TL
import           System.Environment
import qualified Text.Blaze.Html               as H
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              ((!))
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes   as A

wrapper :: H.Html -> H.Html -> H.Html -> H.Html
wrapper title meta body = H.docTypeHtml $ do
    -- Page Head
    H.head $ do
        H.title title
        meta
    -- Page Body
    H.body $ do
        H.main ! A.class_ "content" $ do body
        footer [H.a "Facebook"]


footer :: [H.Html] -> H.Html
footer links = H.footer ! A.class_ "site" $ do
    H.ul ! A.class_ "social" $ forM_ links (H.li)

    H.nav $ do
        H.ul ! A.class_ "nav" $ forM_ ["Home","Projects","Posts"] (H.li)
