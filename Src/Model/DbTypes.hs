{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model.DbTypes where

import           Control.Monad.IO.Class (liftIO)
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.TH

-- Database types
share [mkPersist sqlSettings, mkSave "entityDefs"] [persistLowerCase|

BlogPost
    title       String
    project     ProjectId
    postDate    String
    content     String
    deriving    Show

Author
    fname       String
    lname       String
    profiles    String
    deriving    Show

Project
    name        String
    description String
    deriving    Show

BlogAuthor
    blogPostId  BlogPostId
    authorId    AuthorId
    deriving    Show

|]
