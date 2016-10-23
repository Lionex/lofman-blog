module App.Project
( projectApp
) where

import           API
import           Data.Time
import           Model
import           Servant

projectApp :: Server ProjectAPI
projectApp =
    getProjects :<|>
    getProject  :<|>
    postProject

-- getProjects :: Handler [Projects]
getProjects = return [project]

-- getProject :: ProjectId -> Handler Project
getProject _ = return project

-- postProject :: Project -> Handler Project
postProject _ = return project

project = Project "proj" "desc"
