{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module App.Crud
( crudApp
) where

import           API
import           Control.Monad.Trans
import           Data.Time
import           Database.Persist
import           Database.Persist.Postgresql
import           Model
import           Servant

type Pool = ConnectionPool

crudApp :: (PersistEntity entity, PersistEntityBackend entity ~ SqlBackend) =>
            Pool -> Server (CrudAPI entity)
crudApp pool =
    getEntity  pool :<|>
    postEntity pool

getEntity :: (PersistEntity entity, PersistEntityBackend entity ~ SqlBackend) =>
             Pool -> Maybe (Key entity) -> Handler (MaybeOneOrMany entity)
getEntity pool Nothing    = liftIO $ flip runSqlPersistMPool pool $ do
    entities <- selectList [] []
    return $ Right $ map (entityVal) entities
getEntity pool (Just key) = liftIO $ flip runSqlPersistMPool pool $ do
    mEntity <- get key
    return $ Left mEntity

postEntity :: (PersistEntity entity, PersistEntityBackend entity ~ SqlBackend) =>
              Pool -> entity -> Handler (Maybe entity)
postEntity pool entity = liftIO $ flip runSqlPersistMPool pool $ do
    entityId <- insert entity
    mEntity <- get entityId
    return $ mEntity
