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
-- * content types
( BlogPost
, Author
, Project
-- * utility types
, BlogAuthor
)

import           Control.Monad.IO.Class (liftIO)
import           Data.Time              (UTCTime, DiffTime)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           GHC.Generics

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
    deriving    Show, Generic

Author
    fname       String
    lname       String
    profile     String
    pictureURL  String
    deriving    Show, Generic

Project
    name        String
    description String
    postDate    UTCTime
    deriving    Show, Generic

BlogAuthor
    blogPostId  BlogPostId
    authorId    AuthorId
    category    String
    deriving    Show, Generic

|]
