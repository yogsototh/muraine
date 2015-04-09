{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import           Data.Conduit
import qualified Data.HashMap.Strict as H
import Data.Aeson
import Data.Vector ((!?))
import Data.Text (Text)
import qualified Data.Text as T

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
         [user,pass] -> getEvents user pass Nothing 100000 Nothing
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

anArray :: Value -> Maybe Array
anArray (Array a) = Just a
anArray _ = Nothing

anObject :: Value -> Maybe Object
anObject (Object a) = Just a
anObject _ = Nothing

aString :: Value -> Maybe Text
aString (String a) = Just a
aString _ = Nothing

getFirstId :: LZ.ByteString -> Maybe Text
getFirstId body = decode body >>= anArray >>= (!? 0) >>= anObject >>= H.lookup "id" >>= aString

getTimeAndEtagFromResponse :: Int
                           -> Maybe B.ByteString
                           -> Response LZ.ByteString
                           -> Double
                           -> Maybe Text
                           -> IO (Int, Maybe B.ByteString,Maybe Text)
getTimeAndEtagFromResponse oldTime etag response req_time oldFirstId =
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
                firstId = getFirstId (responseBody response)
            publish (responseBody response) oldFirstId
            return (timeToWaitIn_us,etagResponded,firstId)
        else do
            putStrLn (if notModified304 == responseStatus response
                        then "Nothing changed"
                        else "Something went wrong")
            return (oldTime,etag,oldFirstId)

getEvents :: String             -- ^ Github username
          -> String             -- ^ Github password
          -> Maybe B.ByteString -- ^ ETag
          -> Int                -- ^ Time to wait in micro seconds
          -> Maybe Text         -- ^ First Event Id
          -> IO ()
getEvents user pass etag t oldFirstId = do
    -- Call /events on github
    (req_time, response) <- time (httpGHEvents user pass etag)
    (timeToWaitIn_us,etagResponded,firstId) <- getTimeAndEtagFromResponse t etag response req_time oldFirstId
    threadDelay timeToWaitIn_us
    getEvents user pass etagResponded timeToWaitIn_us firstId

-- takeUpUntil :: Maybe Text -> Maybe Value  -> Maybe [Value]
-- takeUpUntil firstId = error "TODO"

publish :: LZ.ByteString -> Maybe Text -> IO ()
publish body firstId = do
    -- let mevents = decode body >>= anArray >>= takeUpUntil firstId
    -- case mevents of
    --     Just events -> mapM_ (publishOneEvent firstId) events
    --     _ -> return ()
    case firstId of
         Just txt -> putStrLn (T.unpack txt)
         _ -> return ()
    (LZ.putStrLn . LZ.take 40) body

-- publishOneEvent :: Value -> IO ()
-- publishOneEvent mEvent = putStrLn "TODO: publish to Kafka"
