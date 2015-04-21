{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import           System.Environment         (getArgs)
import           System.Exit                (exitFailure)
import qualified Data.ByteString.Lazy.Char8 as LZ
import qualified Network.Nats as Nats
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

main :: IO ()
main = do
     args <- getArgs
     case args of
          [topic] -> do
            nats <- Nats.connect "nats://localhost:4222"
            _ <- Nats.subscribe nats topic Nothing $ \_ _ msg _ -> LZ.putStrLn msg
            forever (threadDelay 1000000 >> putStr ".")
          _ -> showHelpAndExit


--------------------------------------------------------------------------------
showHelpAndExit :: IO ()
showHelpAndExit = do
    hPutStrLn stderr "provide your github username and password please"
    exitFailure
