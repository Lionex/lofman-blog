{-# LANGUAGE OverloadedStrings #-}

module Web.View.Style where

import           Clay
import           Data.Monoid
import           Prelude               hiding ((**))
import           Web.View.Style.Common

siteNav :: Css
siteNav = do
    nav # ".site-nav" ? do
        left    (px 0)
        right   (px 0)
        zeroMargin
        padding_ (em 0.5)

    ul # ".nav" ? do
        display flex
        zeroMargin
        zeroPad
        width   (pct 100)

        li ? listStyleType none

    li # ".nav-button" ? do
        padding        (em 0.5) (em 0.5) (em 0.5) (em 0.5)

        -- The text formatting of links inside nav buttons
        a # ":hover" ? do
            color          grey
            -- flex
        (a <> (a # ":visited")) ? do
            textDecoration none
            color          black

mastHead :: Css
mastHead = header # ".mastHead" ? do
    background white
    height     (px 100)

noMastHead :: Css
noMastHead = header # ".mastHead" ? display none

footer_ :: Css
footer_ = do
    profile

profile :: Css
profile = aside # ".profile" ? do
    background grey

layout :: Css
layout = do
    (body <> main_) ? zeroPad
    body ? do
        zeroMargin
        background bgColor
    main_ ? do
        margin     (em 0) (em 1) (em 0) (em 1)

baseStyle :: Css
baseStyle = do
    siteNav
    layout
    footer_
