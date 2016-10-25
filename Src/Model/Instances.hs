{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric     #-}

module Model.Instances where

import           Data.Aeson
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           GHC.Generics
import           Model.DbTypes
import           Servant
import           Servant.Elm

instance ToJSON   BlogPost
instance FromJSON BlogPost
instance ElmType  BlogPost

instance ToJSON   Project
instance FromJSON Project
instance ElmType  Project

instance ToJSON   Author
instance FromJSON Author
instance ElmType  Author

instance ToJSON   BlogAuthor
instance FromJSON BlogAuthor
