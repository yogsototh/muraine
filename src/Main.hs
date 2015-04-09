{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import           Data.Conduit
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Header (Header,RequestHeaders)
import           Network.HTTP.Types.Status (statusIsSuccessful,notModified304)
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

httpCall :: String -> RequestHeaders -> IO (Response LZ.ByteString)
httpCall url headers = do
    r <- parseUrl url
    let request = r { requestHeaders = headers }
    withManager (httpLbs request)

authHttpCall :: String -> String -> String -> RequestHeaders -> IO (Response LZ.ByteString)
authHttpCall url user pass headers = do
    r <- parseUrl url
    let request = r {requestHeaders = headers }
        requestWithAuth = applyBasicAuth (B.pack user) (B.pack pass) request
    withManager (httpLbs requestWithAuth)

httpGHEvents :: String -> String -> Maybe B.ByteString -> IO (Response LZ.ByteString)
httpGHEvents user pass etag = authHttpCall  "https://api.github.com/events" user pass headers
      where
          headers = ("User-Agent","HTTP-Conduit"):maybe [] (\e -> [("If-None-Match",B.tail (B.tail e))]) etag

showHeader :: Header -> IO ()
showHeader (name, value) = B.putStrLn (original name <> ": " <> value)

showHelpAndExit :: IO ()
showHelpAndExit = do
    putStrLn "provide your github username and password please"
    exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
         [user,pass] -> getEvents user pass Nothing 100000
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

getTimeAndEtagFromResponse :: Int
                           -> Maybe B.ByteString
                           -> Response LZ.ByteString
                           -> Double
                           -> IO (Int, Maybe B.ByteString)
getTimeAndEtagFromResponse oldTime etag response req_time =
    if statusIsSuccessful (responseStatus response)
        then do
            let headers = responseHeaders response
            -- If the server returned a date we use it
            -- otherwise we use the local current time
            serverDateEpoch <- case lookup "Date" headers of
                                Nothing -> fmap round getPOSIXTime
                                Just d -> return (epochFromString (B.unpack d))
            let etagResponded = lookup "ETag" headers
                remainingHeader = lookup "X-RateLimit-Remaining" headers
                remaining = maybe 1 (read . B.unpack) remainingHeader
                resetHeader = lookup "X-RateLimit-Reset" headers
                reset = maybe 1 (read . B.unpack) resetHeader
                timeBeforeReset = reset - serverDateEpoch
                t = 1000000 * timeBeforeReset `div` remaining
                timeToWaitIn_us = max 0 (t - floor (1000000 * req_time))
            -- TODO: read all pages until we reach the first ID of the first page
            -- of the preceeding loop
            publish (responseBody response)
            return (timeToWaitIn_us,etagResponded)
        else do
            putStrLn (if notModified304 == responseStatus response
                        then "Nothing changed"
                        else "Something went wrong")
            return (oldTime,etag)

getEvents :: String             -- ^ Github username
          -> String             -- ^ Github password
          -> Maybe B.ByteString -- ^ ETag
          -> Int                -- ^ Time to wait in micro seconds
          -> IO ()
getEvents user pass etag t = do
    -- Call /events on github
    (req_time, response) <- time (httpGHEvents user pass etag)
    (timeToWaitIn_us,etagResponded) <- getTimeAndEtagFromResponse t etag response req_time
    threadDelay timeToWaitIn_us
    getEvents user pass etagResponded timeToWaitIn_us

publish :: LZ.ByteString -> IO ()
publish = LZ.putStrLn . LZ.take 40
