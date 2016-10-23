module App
( app
, apiServer
) where

import           API
import           App.Author
import           App.Blog
import           App.Project
import           Model
import           Network.Wai
import           Servant

app :: IO Application
app = serve api <$> server

server :: IO (Server API)
server = do
    return $ apiServer :<|> serveDirectory "static"

apiServer =
    blogApp :<|>
    projectApp :<|>
    authorApp
