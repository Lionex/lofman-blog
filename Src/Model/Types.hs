module Model.Types
( Page (..)
) where

-- | Datatype that represents pages not generated from database types.
data Page
    = Home     -- ^ Homepage
    | Error404 -- ^ Error page
    deriving (Eq, Show)
