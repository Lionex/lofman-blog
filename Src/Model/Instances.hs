module Model.Instances where

import           Data.Aeson
import           GHC.Generics
import           Model.DbTypes

instance ToJSON   BlogPost
instance FromJSON BlogPost

instance ElmType  Project
instance ToJSON   Project
instance FromJSON Project

instance ToJSON   Author
instance FromJSON Author

instance ToJSON   BlogAuthor
instance FromJSON BlogAuthor
