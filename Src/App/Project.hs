module App.Project
( projectApp
) where

import           API
import           App.Crud
import           Database.Persist.Postgresql (ConnectionPool)
import           Servant

projectApp :: ConnectionPool -> Server ProjectAPI
projectApp = crudApp
