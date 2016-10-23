{-# LANGUAGE FlexibleInstances #-}

module Model.Instances where

import           Data.Aeson
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           GHC.Generics
import           Model.DbTypes
import           Servant


instance ToJSON   BlogPost
instance FromJSON BlogPost

instance FromText (BlogPostId) where
    fromText = fmap toSqlKey . fromText

instance ToJSON   Project
instance FromJSON Project

instance FromText (ProjectId) where
    fromText = fmap toSqlKey . fromText

instance ToJSON   Author
instance FromJSON Author

instance FromText (AuthorId) where
    fromText = fmap toSqlKey . fromText


instance ToJSON   BlogAuthor
instance FromJSON BlogAuthor
