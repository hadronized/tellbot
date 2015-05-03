module HTML where

import Control.Exception ( SomeException, catch )
import Control.Monad ( guard )
import Data.List ( isPrefixOf )
import Network.HTTP.Conduit
import Text.Regex.Posix ( (=~) )

htmlTitle :: FilePath -> String -> IO (Maybe String)
htmlTitle regPath url = do
    regexps <- flip catch handleException . fmap lines $ readFile regPath 
    print regexps
    if (safeHost regexps url) then do
      title <- flip catch handleException $ fmap (extractTitle . show) $ simpleHttp httpPrefixedURL
      case title of
        Just _ -> pure title
        Nothing -> flip catch handleException $ fmap (extractTitle . show) $ simpleHttp httpsPrefixedURL
      else
        pure Nothing
  where
    httpPrefixedURL = if "http://" `isPrefixOf` url then url else "http://" ++ url
    httpsPrefixedURL = if "https://" `isPrefixOf` url then url else "https://" ++ url
    handleException :: (Monoid m) => SomeException -> IO m
    handleException _ = pure mempty
    
extractTitle :: String -> Maybe String
extractTitle body = do
    guard (not $ null titleHTML)
    pure $ dropAround (length "<title>") (length "</title>") titleHTML
  where
    titleHTML :: String
    titleHTML = body =~ "<title>[^<]*</title>"

dropAround :: Int -> Int -> String -> String
dropAround s e = reverse . drop e . reverse . drop s

-- Filter an URL so that we don’t make overviews of unknown hosts. Pretty
-- cool to prevent people from going onto sensitive websites.
--
-- All the regex should be put in a file. One per row.
safeHost :: [String] -> String -> Bool
safeHost regexps url = any (url =~) regexps
