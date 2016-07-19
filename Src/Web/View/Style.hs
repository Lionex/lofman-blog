{-# LANGUAGE OverloadedStrings #-}

module Web.View.Style where
-- Defines style elements that correspond to specific structural elements 
-- of the page content.
-- Stylesheets for specific sections of the site are the result of
-- combining these smaller style sheets.

import           Clay
import           Data.Monoid
import           Prelude               hiding ((**))
import           Web.View.Style.Common

-----------------------------------------------------------------------
-- Header & Navigation

-- Defines the structural presentation of the navigation bar.
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

        (a <> (a # ":visited")) ? do
            textDecoration none
            color          black

mastHead :: Css
mastHead = header # ".mastHead" ? do
    background white
    height     (px 100)

noMastHead :: Css
noMastHead = header # ".mastHead" ? display none

-----------------------------------------------------------------------
-- Foot

-- Defines the structural presentation of the elemtns fo the footer.
footer_ :: Css
footer_ = do
    profile

-- Contains style rules for the profile displayed in the footer.
profile :: Css
profile = aside # ".profile" ? do
    background grey

-----------------------------------------------------------------------
-- Layout

-- Defines the positional and structural relationship between page
-- elements.
layout :: Css
layout = do
    (body <> main_) ? zeroPad
    body ? do
        zeroMargin
        background bgColor
    main_ ? do
        margin     (em 0) (em 1) (em 0) (em 1)


-----------------------------------------------------------------------
-- Sheets

-- Style rules for the bare minimum elements which are guaranteed to
-- appear on the site.
baseStyle :: Css
baseStyle = do
    siteNav
    layout
    footer_
