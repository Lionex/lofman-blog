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

instance ToJSON   Project
instance FromJSON Project

instance ToJSON   Author
instance FromJSON Author

instance ToJSON   BlogAuthor
instance FromJSON BlogAuthor
