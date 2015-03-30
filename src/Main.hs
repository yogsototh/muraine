{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import           Data.Conduit
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Header (Header)
import           Network.HTTP.Types.Status (statusIsSuccessful)
import qualified Data.ByteString.Lazy.Char8 as LZ
import qualified Data.ByteString.Char8 as B
import Data.CaseInsensitive (original)
import Data.Monoid ((<>))
import System.Environment (getArgs)
import System.Exit (exitFailure)

simpleHTTPWithUserAgent :: String -> String -> String -> IO (Response LZ.ByteString)
simpleHTTPWithUserAgent url user pass = do
    r <- parseUrl url
    let request = r {requestHeaders = [("User-Agent","HTTP-Conduit")]}
        requestWithAuth = applyBasicAuth (B.pack user) (B.pack pass) request
    withManager (httpLbs requestWithAuth)

showHeader :: Header -> IO ()
showHeader (name, value) = B.putStrLn $ original name <> ": " <> value

showHelpAndExit :: IO ()
showHelpAndExit = do
    putStrLn "provide your github username and password please"
    exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
         [user,pass] -> continueWithUserAndPass user pass
         _ -> showHelpAndExit

continueWithUserAndPass :: String -> String -> IO ()
continueWithUserAndPass user pass = do
    response <- simpleHTTPWithUserAgent "https://api.github.com/events" user pass
    if statusIsSuccessful (responseStatus response)
       then do
              LZ.putStrLn (responseBody response)
              mapM_ showHeader (responseHeaders response)
       else
              putStrLn "Something went wrong"
