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

-- Helper style rules
-- Designed for compositional use and utility
zeroPad :: Css
zeroPad = padding (em 0) (em 0) (em 0) (em 0)

zeroMargin :: Css
zeroMargin = margin (em 0) (em 0) (em 0) (em 0)

margin_ :: Size a -> Css
margin_ n = margin n n n n

padding_ :: Size a -> Css
padding_ n = padding n n n n
