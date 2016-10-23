module Main
( main
) where

import           API
import           App
import           Network.Wai
import           Network.Wai.Handler.Warp
import           System.Environment

main :: IO ()
main = do
    -- Read environment variables
    port <- getEnv "PORT"
    -- Run the app
    run (read port) =<< app
