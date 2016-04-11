module DB where

-- |

import           Data.Default                       (def)
import qualified Data.Text                   as T
import           Data.Text.Encoding                 (encodeUtf8)
import           Data.Text.Lazy                     (Text)

import qualified Database.Persist            as DB
import qualified Database.Persist.Postgresql as DB

import Web.Heroku                                   (parseDatabaseUrl)

import           DB.Models
import           DB.Common


-- | Allows for parsing of environment variables.
-- Allows for different application configuration depending Environment
data Environment
    = Development
    | Production
    | Test
    deriving (Show, Read, Eq)

runDB = putStrLn "Database code"
