module Model.Types
( Page (..)
) where

data Page
    = Home
    | Error404
    deriving (Eq, Show)
