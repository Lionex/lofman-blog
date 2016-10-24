{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module API
-- * API types
( API
, API'
, AuthorAPI
, BlogAPI
, ProjectAPI
-- * API function for server
, api
) where

import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Model
import           Servant.API

type BlogAPI = "blog" :>
    (    Get '[JSON] [BlogPost]
    :<|> QueryParam "b" BlogPostId :> Get '[JSON] BlogPost
    :<|> ReqBody '[JSON] BlogPost :> Post '[JSON] BlogPost
    )

type ProjectAPI = "project" :>
    (    Get '[JSON] [Project]
    :<|> QueryParam "p" ProjectId :> Get '[JSON] Project
    :<|> ReqBody '[JSON] Project :> Post '[JSON] Project
    )

type AuthorAPI = "author" :>
    (    Get '[JSON] [Author]
    :<|> QueryParam "a" AuthorId :> Get '[JSON] Author
    :<|> ReqBody '[JSON] Author :> Post '[JSON] Author
    )

type API' = "api" :>
    (    BlogAPI
    :<|> ProjectAPI
    :<|> AuthorAPI
    )

type API = API' :<|> Raw

api :: Proxy API
api = Proxy
