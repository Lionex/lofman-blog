{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative                (Applicative)
import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Control.Monad.Logger               (runNoLoggingT,
    runStdoutLoggingT)
import           Control.Monad.Reader               (MonadReader, ReaderT,
    asks, runReaderT)
import           Control.Monad.Trans.Class          (MonadTrans, lift)

import Lib


-- | Gets proper config and then runs the application.
main :: IO ()
main = runAppT
