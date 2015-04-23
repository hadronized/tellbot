module HTML where

import Control.Exception ( SomeException, catch )
import Control.Monad ( guard )
import Data.List ( isPrefixOf )
import Network.HTTP ( getRequest, getResponseBody, simpleHTTP )
import Text.Regex.Posix ( (=~) )

htmlTitle :: String -> IO (Maybe String)
htmlTitle url = flip catch handleException $
    fmap extractTitle $ simpleHTTP (getRequest prefixedURL) >>= getResponseBody
  where
    prefixedURL = if "http://" `isPrefixOf` url || "https://" `isPrefixOf` url then url else "http://" ++ url
    handleException :: SomeException -> IO (Maybe String)
    handleException _ = pure Nothing
    
extractTitle :: String -> Maybe String
extractTitle body = do
    guard (not $ null titleHTML)
    pure $ dropAround (length "<title>") (length "</title>") titleHTML
  where
    titleHTML :: String
    titleHTML = body =~ "<title>[^<]*</title>"

dropAround :: Int -> Int -> String -> String
dropAround s e = reverse . drop e . reverse . drop s
