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
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Time.Format (readTime)
import Data.Time.Clock.POSIX (getPOSIXTime,utcTimeToPOSIXSeconds)
import System.Locale (defaultTimeLocale)
import System.CPUTime (getCPUTime)

simpleHTTPWithUserAgent :: String -> String -> String -> IO (Response LZ.ByteString)
simpleHTTPWithUserAgent url user pass = do
    r <- parseUrl url
    let request = r {requestHeaders = [("User-Agent","HTTP-Conduit")]}
        requestWithAuth = applyBasicAuth (B.pack user) (B.pack pass) request
    withManager (httpLbs requestWithAuth)

simpleHTTPWithUserAgentETag :: String -> String -> String -> Maybe B.ByteString -> IO (Response LZ.ByteString)
simpleHTTPWithUserAgentETag url user pass etag = do
    r <- parseUrl url
    let request = r {requestHeaders = ("User-Agent","HTTP-Conduit"):
                                      maybe [] (\e -> [("If-None-Match",B.tail (B.tail e))]) etag}
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
         [user,pass] -> getEvents user pass Nothing
         _ -> showHelpAndExit

rfc822DateFormat :: String
rfc822DateFormat = "%a, %_d %b %Y %H:%M:%S %Z"

epochFromString :: String -> Int
epochFromString = floor . utcTimeToPOSIXSeconds . readTime defaultTimeLocale rfc822DateFormat

time :: IO a -> IO (Double, a)
time action = do
    startTime <- getCPUTime
    res <- action
    endTime <- getCPUTime
    return (fromIntegral (endTime - startTime)/(10**12),res)

getEvents :: String             -- ^ Github username
          -> String             -- ^ Github password
          -> Maybe B.ByteString -- ^ ETag
          -> IO ()
getEvents user pass etag = do
    (req_time, response) <- time $ simpleHTTPWithUserAgentETag "https://api.github.com/events" user pass etag
    if statusIsSuccessful (responseStatus response)
        then do
            let headers = responseHeaders response
            -- If the server returned a date we use it
            -- otherwise we use the local current time
            serverDateEpoch <- case lookup "Date" headers of
                                Nothing -> getPOSIXTime >>= \t -> return ( round t)
                                Just d -> return $ epochFromString (B.unpack d)
            let etagResponded = lookup "ETag" headers
                remaining = maybe 1 (read . B.unpack) $ lookup "X-RateLimit-Remaining" headers
                reset = maybe 1 (read . B.unpack) $ lookup "X-RateLimit-Reset" headers
                timeBeforeReset = reset - serverDateEpoch
                t = 1000000 * timeBeforeReset `div` remaining
                timeToWaitIn_us = max 0 (t - floor (1000000 * req_time))
            print req_time
            print timeToWaitIn_us
            publish (responseBody response)
            threadDelay timeToWaitIn_us
            getEvents user pass etagResponded
        else
            putStrLn "Something went wrong"

publish :: LZ.ByteString -> IO ()
publish = LZ.putStrLn . LZ.take 40
