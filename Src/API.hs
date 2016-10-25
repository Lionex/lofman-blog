{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module API
-- * API types
( API
, API'
, AuthorAPI
, BlogAPI
, ProjectAPI
, CrudAPI
-- * Utility Types
, MaybeOneOrMany
-- * API function for server
, api
) where

import           Control.Monad.Trans.Either (EitherT)
import           Data.Aeson
import           Data.Proxy
import           Database.Persist
import           GHC.Generics
import           Model
import           Servant                    (ServantErr)
import           Servant.API

type CrudAPI entity =
    QueryParam "b" (Key entity) :> Get  '[JSON] (MaybeOneOrMany entity)
    :<|> ReqBody '[JSON] entity :> Post '[JSON] (Maybe entity)

type BlogAPI    = "blog"    :> CrudAPI BlogPost
type ProjectAPI = "project" :> CrudAPI Project
type AuthorAPI  = "author"  :> CrudAPI Author

type API' = "api" :>
    (    BlogAPI
    :<|> ProjectAPI
    :<|> AuthorAPI
    )

type API = API' :<|> Raw

type MaybeOneOrMany a = Either (Maybe a) [a]

api :: Proxy API
api = Proxy
