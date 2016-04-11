{-# LANGUAGE OverloadedStrings #-}

-- | Library of functions and types for running a blog server.
module Lib
    ( runAppT
    ) where

-- Web server
import Web.Scotty

-- Database code library
import DB


runAppT :: IO ()
runAppT = putStrLn "running app :/"
