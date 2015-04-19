{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import           Data.Conduit
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.HashMap.Strict        as H
import           Data.Maybe                 (isNothing)
import           Data.Text                  (Text)
import           Data.Vector                (Vector, (!?))
import qualified Data.Vector                as V
import           System.IO

import           Control.Concurrent         (threadDelay)
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as LZ
import           Data.CaseInsensitive       (original)
import           Data.Monoid                ((<>))
import           Data.Time.Clock.POSIX      (getPOSIXTime,
                                             utcTimeToPOSIXSeconds)
import           Data.Time.Format           (readTime)
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Header  (Header, RequestHeaders,
                                             ResponseHeaders)
import           Network.HTTP.Types.Status  (notModified304, statusIsSuccessful)
import           System.CPUTime             (getCPUTime)
import           System.Environment         (getArgs)
import           System.Exit                (exitFailure)
import           System.Locale              (defaultTimeLocale)

-- Datas CallInfo contains all necessary information to manage events searching
data CallInfo = CallInfo { _user            :: String             -- ^ Github username
                         , _pass            :: String             -- ^ Github password
                         , _etag            :: Maybe B.ByteString -- ^ ETag
                         , _timeToWait      :: Int                -- ^ Time to wait in micro seconds
                         , _firstId         :: Maybe Text         -- ^ First Event Id
                         , _lastId          :: Maybe Text         -- ^ Last  Event Id
                         , _searchedFirstId :: Maybe Text         -- ^ First Event Id searched
                         , _page            :: Int                -- ^ Page
                         } deriving (Show)

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
         [user,pass] -> let initCallInfo = CallInfo { _user = user
                                                    , _pass = pass
                                                    , _etag = Nothing
                                                    , _timeToWait = 100000
                                                    , _firstId = Nothing
                                                    , _lastId = Nothing
                                                    , _searchedFirstId = Nothing
                                                    , _page = 1
                                                    }
                        in iterateM_ getEvents initCallInfo
         _ -> showHelpAndExit

--------------------------------------------------------------------------------
showHelpAndExit :: IO ()
showHelpAndExit = do
    hPutStrLn stderr "provide your github username and password please"
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
    let request = r {requestHeaders = headers, checkStatus = \_ _ _ -> Nothing}
        requestWithParams = setQueryString params request
        requestWithAuth = applyBasicAuth (B.pack user) (B.pack pass) requestWithParams
    withManager (httpLbs requestWithAuth)

httpGHEvents :: String
                -> String
                -> Maybe B.ByteString
                -> Int
                -> IO (Response LZ.ByteString)
httpGHEvents user pass etag page =
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

safeVLast :: Vector a -> Maybe a
safeVLast v = if V.null v then Nothing else Just (V.last v)

getLastId :: Maybe Value -> Maybe Text
getLastId events = events >>= anArray >>= safeVLast >>= anObject
                    >>= H.lookup "id" >>= aString
--------------------------------------------------------------------------------
-- The Algorithm of getting events
--------------------------------------------------------------------------------

-- | Iterate in a monad, mainly the safe as:
--
-- @
-- x1 <- f x0
-- x2 <- f x1
-- ...
-- @
iterateM_ :: Monad m => (a -> m a) -> a -> m ()
iterateM_ f x = f x >>= iterateM_ f

-- | Getting events return the next CallInfo for the next call
getEvents :: CallInfo -> IO CallInfo
getEvents callInfo = do
  -- Call /events on github
  (req_time, response) <- time (httpGHEvents (_user callInfo)
                                             (_pass callInfo)
                                             (_etag callInfo)
                                             (_page callInfo))
  newCallInfo <- getCallInfoFromResponse callInfo response req_time
  threadDelay (_timeToWait newCallInfo)
  return newCallInfo

-- | the time to wait between two HTTP calls using headers informations
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

-- | given an HTTP response compute the next CallInfo and also publish events
-- Beware, this is an heuristic with the hypothesis that there is no more than
-- one page to download, by one page.
getCallInfoFromResponse :: CallInfo -> Response LZ.ByteString -> Double -> IO CallInfo
getCallInfoFromResponse callInfo response req_time =
    if statusIsSuccessful (responseStatus response)
        then do
            let headers = responseHeaders response
            t <- getTimeToWaitFromHeaders headers
            let
              -- Time to wait is time between two HTTP call minus the time
              -- the last HTTP call took to answer
              timeToWaitIn_us = max 0 (t - floor (1000000 * req_time))
              events = decode (responseBody response)
              nextFirstId = if _page callInfo == 1 || isNothing (_firstId callInfo)
                              then getFirstId events
                              else _firstId callInfo
              nextLastId = getLastId events
              containsSearchedFirstId = containsId (_searchedFirstId callInfo) events
              etagResponded = lookup "ETag" headers
              -- Read next pages until we reach the old first ID of the first page
              -- of the preceeding loop
              -- return a new page if the first ID wasn't found
              nextPage = if containsSearchedFirstId || (_page callInfo >= 10)
                          then 1
                          else _page callInfo + 1
              nextSearchedFirstId = if containsSearchedFirstId || (_page callInfo >= 10)
                                      then nextFirstId
                                      else _searchedFirstId callInfo
            publish events (_searchedFirstId callInfo) (_lastId callInfo)
            return (callInfo { _firstId = nextFirstId
                             , _lastId = nextLastId
                             , _page = nextPage
                             , _etag = etagResponded
                             , _timeToWait = timeToWaitIn_us
                             , _searchedFirstId = nextSearchedFirstId
                             })
        else do
            hPutStrLn stderr (if notModified304 == responseStatus response
                              then "Nothing changed"
                              else "Something went wrong")
            return callInfo

selectEvents :: Maybe Value -> Maybe Text -> Maybe Text -> [Value]
selectEvents events firstId lastId = case events of
          Nothing -> []
          Just evts -> (takeWhile ((/= firstId) . getId)
                        . guillotine ((== lastId) . getId)
                        . removeOld) evts
  where
      getId e = e ^? key "id" . _String
      guillotine :: (a -> Bool) -> [a] -> [a]
      guillotine _ [] = []
      guillotine f (x:xs) = if f x then xs else x:xs
      removeOld evts = case lastId of
        Nothing -> evts ^.. values
        Just _  -> if containsId lastId (Just evts)
                      then dropWhile ((/= lastId) . getId) (evts ^.. values)
                      else evts ^.. values

publish :: Maybe Value -> Maybe Text -> Maybe Text -> IO ()
publish events firstId lastId = mapM_ publishOneEvent (selectEvents events firstId lastId)

publishOneEvent :: Value -> IO ()
publishOneEvent = LZ.putStrLn . encode -- . (^? key "id")
