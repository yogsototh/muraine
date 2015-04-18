{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import           Data.Conduit
import qualified Data.HashMap.Strict as H
import Data.Aeson
import Control.Lens
import Data.Aeson.Lens
import Data.Vector ((!?))
import Data.Text (Text)
import Data.Maybe (isNothing)

import           Network.HTTP.Conduit
import           Network.HTTP.Types.Header (Header,RequestHeaders,ResponseHeaders)
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


data CallInfo = CallInfo { _user :: String            -- ^ Github username
                         , _pass :: String            -- ^ Github password
                         , _etag :: Maybe B.ByteString  -- ^ ETag
                         , _timeToWait :: Int         -- ^ Time to wait in micro seconds
                         , _firstId :: Maybe Text     -- ^ First Event Id
                         , _searchedFirstId :: Maybe Text -- ^ First Event Id searched
                         , _page :: Int               -- ^ Page
                         } deriving (Show)
--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
         [user,pass] -> getEvents (CallInfo user pass Nothing 100000 Nothing Nothing 1)
         _ -> showHelpAndExit

--------------------------------------------------------------------------------
showHelpAndExit :: IO ()
showHelpAndExit = do
    putStrLn "provide your github username and password please"
    exitFailure

--------------------------------------------------------------------------------
-- HTTP Calls
--------------------------------------------------------------------------------

authHttpCall :: String
                -> String
                -> String
                -> RequestHeaders
                -> [(B.ByteString,Maybe B.ByteString)]
                -> IO (Response LZ.ByteString)
authHttpCall url user pass headers params = do
    r <- parseUrl url
    let request = r {requestHeaders = headers}
        requestWithParams = setQueryString params request
        requestWithAuth = applyBasicAuth (B.pack user) (B.pack pass) requestWithParams
    withManager (httpLbs requestWithAuth)

httpGHEvents :: String
                -> String
                -> Maybe B.ByteString
                -> Int
                -> IO (Response LZ.ByteString)
httpGHEvents user pass etag page = do
    print params
    authHttpCall "https://api.github.com/events" user pass headers params
      where
          headers = ("User-Agent","HTTP-Conduit"):
                    maybe [] (\e -> [("If-None-Match",B.tail (B.tail e))]) etag
          params = [("page",Just (B.pack (show page)))]

--------------------------------------------------------------------------------
-- Date
--------------------------------------------------------------------------------

showHeader :: Header -> IO ()
showHeader (name, value) = B.putStrLn (original name <> ": " <> value)

rfc822DateFormat :: String
rfc822DateFormat = "%a, %_d %b %Y %H:%M:%S %Z"

epochFromString :: String -> Int
epochFromString = floor . utcTimeToPOSIXSeconds . readTime defaultTimeLocale rfc822DateFormat

--------------------------------------------------------------------------------
-- Count how long a function evaluation took
--------------------------------------------------------------------------------

time :: IO a -> IO (Double, a)
time action = do
    startTime <- getCPUTime
    res <- action
    endTime <- getCPUTime
    return (fromIntegral (endTime - startTime)/(10**12),res)

--------------------------------------------------------------------------------
-- JSON
--------------------------------------------------------------------------------

anArray :: Value -> Maybe Array
anArray (Array a) = Just a
anArray _ = Nothing

anObject :: Value -> Maybe Object
anObject (Object a) = Just a
anObject _ = Nothing

aString :: Value -> Maybe Text
aString (String a) = Just a
aString _ = Nothing

getFirstId :: Maybe Value -> Maybe Text
getFirstId events = events >>= anArray >>= (!? 0) >>= anObject
                    >>= H.lookup "id" >>= aString

--------------------------------------------------------------------------------
-- The Algorithm of getting events
--------------------------------------------------------------------------------

-- | Getting events
getEvents :: CallInfo -> IO ()
getEvents callInfo = do
  -- Call /events on github
  (req_time, response) <- time (httpGHEvents (_user callInfo) (_pass callInfo) (_etag callInfo) (_page callInfo))
  newCallInfo <- getTimeAndEtagFromResponse callInfo response req_time
  threadDelay (_timeToWait newCallInfo)
  getEvents newCallInfo


getTimeToWaitFromHeaders :: ResponseHeaders -> IO Int
getTimeToWaitFromHeaders headers = do
    -- Ask current date only if server didn't reponsded with its own
    serverDateEpoch <- case lookup "Date" headers of
                        Nothing -> fmap round getPOSIXTime
                        Just d -> return (epochFromString (B.unpack d))
    let remainingHeader = lookup "X-RateLimit-Remaining" headers
        remaining = maybe 1 (read . B.unpack) remainingHeader
        resetHeader = lookup "X-RateLimit-Reset" headers
        reset = maybe 1 (read . B.unpack) resetHeader
        timeBeforeReset = reset - serverDateEpoch
    return (1000000 * timeBeforeReset `div` remaining)

-- | Get the list of Ids from the body and check if firstId is in it
containsId :: Maybe Text -> Maybe Value -> Bool
containsId firstId events = maybe False (\i -> Just i `elem` eventsIds) firstId
    where
        eventsIds :: [Maybe Text]
        eventsIds = maybe [] (^.. _Array . traverse . to (^? key "id" . _String)) events

getTimeAndEtagFromResponse :: CallInfo -> Response LZ.ByteString -> Double -> IO CallInfo
getTimeAndEtagFromResponse callInfo response req_time = do
    print callInfo
    if statusIsSuccessful (responseStatus response)
        then do
            let headers = responseHeaders response
            t <- getTimeToWaitFromHeaders headers
            let etagResponded = lookup "ETag" headers
                timeToWaitIn_us = max 0 (t - floor (1000000 * req_time))
                events = decode (responseBody response)
                nextFirstId = if _page callInfo == 1 || isNothing (_firstId callInfo) then getFirstId events else _firstId callInfo
                containsSearchedFirstId = containsId (_searchedFirstId callInfo) events
                -- Read next pages until we reach the old first ID of the first page
                -- of the preceeding loop
                -- return a new page if the first ID wasn't found
                nextPage = if containsSearchedFirstId || (_page callInfo >= 10)
                            then 1
                            else _page callInfo + 1
                nextSearchedFirstId = if containsSearchedFirstId || (_page callInfo >= 10)
                                        then nextFirstId
                                        else _searchedFirstId callInfo
            publish events (_searchedFirstId callInfo)
            return (callInfo {_firstId = nextFirstId, _page = nextPage, _etag = etagResponded, _timeToWait = timeToWaitIn_us, _searchedFirstId = nextSearchedFirstId})
        else do
            putStrLn (if notModified304 == responseStatus response
                        then "Nothing changed"
                        else "Something went wrong")
            return callInfo

publish :: Maybe Value -> Maybe Text -> IO ()
publish events firstId = mapM_ publishOneEvent eventsUntilFirstId
    where
        eventsUntilFirstId = case events of
          Nothing -> []
          Just evts -> takeWhile (\e -> (e ^? key "id" . _String) /= firstId) (evts ^.. values)

publishOneEvent :: Value -> IO ()
publishOneEvent = LZ.putStrLn . encode . (^? key "id")
