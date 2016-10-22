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
import           Data.Time              (UTCTime, DiffTime)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

-- Database types
share [mkPersist sqlSettings, mkSave "entityDefs", mkMigrate "migrateAll"] [persistLowerCase|

BlogPost
    title       String
    postDate    UTCTime
    category    String
    content     String
    project     ProjectId
    pageViews   Int
    readingTime DiffTime
    deriving    Show

Author
    fname       String
    lname       String
    profile     String
    deriving    Show

Project
    name        String
    description String
    postDate    UTCTime
    deriving    Show

BlogAuthor
    blogPostId  BlogPostId
    authorId    AuthorId
    deriving    Show

|]
