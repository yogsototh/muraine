{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import           Data.Conduit
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Header (Header)
import qualified Data.ByteString.Lazy.Char8 as LZ
import qualified Data.ByteString.Char8 as B
import Data.CaseInsensitive (original)
import Data.Monoid ((<>))

simpleHTTPWithUserAgent :: String -> IO (Response LZ.ByteString)
simpleHTTPWithUserAgent url = do
    r <- parseUrl url
    let request = r {requestHeaders = [("User-Agent","HTTP-Conduit")]}
    withManager $ \manager -> httpLbs request manager

showHeader :: Header -> IO ()
showHeader (name, value) = B.putStrLn $ original name <> ": " <> value

main :: IO ()
main = do
    response <- simpleHTTPWithUserAgent "https://api.github.com/events"
    LZ.putStrLn (responseBody response)
    mapM_ showHeader (responseHeaders response)
