{-# LANGUAGE OverloadedStrings #-}

module Web.View.Style.Common where

import           Clay
import           Data.Monoid
import           Prelude hiding ((**))

-- Color Pallete
-- Functions to keep colors in once place.

bgColor :: Color
bgColor = "#f8f8f8"

-- Typography
-- functions which collect style rules for text.

textFont :: Css
textFont = do
    fontSize      (px 20)
    fontFamily    [] [sansSerif]
    textRendering optimizeLegibility
    color         "#222"

headerFont :: Css
headerFont = do
    fontSize      (em 1.5)
    fontFamily    [] [sansSerif]
    textRendering optimizeLegibility
    color         "#222"

-----------------------------------------------------------------------
-- Helper style rules
-- Designed for compositional use and utility

-- Creates a style rule to give zero padding to an element.
zeroPad :: Css
zeroPad = padding (em 0) (em 0) (em 0) (em 0)

-- Creates a style rule to give zero margins to an element.
zeroMargin :: Css
zeroMargin = margin (em 0) (em 0) (em 0) (em 0)

-- Creates a rule with the same margin on all sides.
margin_ :: Size a -> Css
margin_ n = margin n n n n

-- Creates a rule with the same padding on all sides.
padding_ :: Size a -> Css
padding_ n = padding n n n n
