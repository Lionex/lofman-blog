{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}


module App.Author
( authorApp
) where

import           API
import           App.Crud
import           Database.Persist.Postgresql (ConnectionPool)
import           Servant

authorApp :: ConnectionPool -> Server AuthorAPI
authorApp = crudApp
