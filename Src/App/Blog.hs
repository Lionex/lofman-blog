module App.Blog
( blogApp
) where

import           API
import           App.Crud
import           Database.Persist.Postgresql (ConnectionPool)
import           Servant

blogApp :: ConnectionPool -> Server BlogAPI
blogApp = crudApp
