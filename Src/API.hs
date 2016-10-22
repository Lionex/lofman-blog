{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module API
( API
, api
) where

import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Model
import           Servant.API

type BlogAPI = "blog"
    :> Get '[JSON] [BlogPost]
    :<|> ReqBody '[JSON] BlogPost :> Post '[JSON] BlogPost
    :<|> Capture "blogPostId" BlogPostId :> Get '[JSON] BlogPost

type ProjectAPI = "project"
    :> Get '[JSON] [Project]
    :<|> ReqBody '[JSON] Project :> Post '[JSON] Project
    :<|> Capture "projectID" ProjectId :> Get '[JSON] Project

type AuthorAPI = "author"
    :> Get '[JSON] [Author]
    :<|> ReqBody '[JSON] Author :> Post '[JSON] Author
    :<|> Capture "authorId" AuthorId :> Get '[JSON] Author

type API' = "api"
    :> BlogAPI
    :<|> ProjectAPI
    :<|> AuthorAPI

type API = API' :<|> Raw

api :: Proxy API
api = Proxy
