module HTML where

import Control.Applicative ( pure )
import Control.Exception ( SomeException, catch )
import Control.Monad ( guard )
import Data.List ( isPrefixOf )
import Network.HTTP.Conduit
import Text.Regex.Posix ( (=~) )

htmlTitle :: String -> IO (Maybe String)
htmlTitle url = do
    -- test http; if it fails, test https
    http <- flip catch handleException $ fmap (extractTitle . show) $ simpleHttp httpPrefixedURL
    case http of
      Just _ -> pure http
      Nothing -> flip catch handleException $ fmap (extractTitle . show) $ simpleHttp httpsPrefixedURL
  where
    httpPrefixedURL = if "http://" `isPrefixOf` url then url else "http://" ++ url
    httpsPrefixedURL = if "https://" `isPrefixOf` url then url else "https://" ++ url
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
