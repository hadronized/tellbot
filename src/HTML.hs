module HTML where

import Control.Exception ( SomeException, catch )
import Control.Monad ( guard )
import Data.ByteString.Lazy ( toStrict )
import Data.List ( isPrefixOf )
import Network.HTTP.Conduit
import Data.Text as T ( drop, dropEnd, pack, strip, unpack )
import Data.Text.Encoding ( decodeUtf8 )
import Text.HTML.TagSoup
import Text.Regex.Posix ( (=~) )

htmlTitle :: FilePath -> String -> IO (Maybe String)
htmlTitle regPath url = do
    regexps <- flip catch handleException . fmap lines $ readFile regPath 
    print regexps
    if (safeHost regexps url) then do
      title <- flip catch handleException $ fmap (extractTitle . concat . lines . unpack . decodeUtf8 . toStrict) $ simpleHttp httpPrefixedURL
      case title of
        Just _ -> pure title
        Nothing -> flip catch handleException $ fmap (extractTitle . unpack . decodeUtf8 . toStrict) $ simpleHttp httpsPrefixedURL
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
    pure . escape . chomp $ removeMarker titleHTML
  where
    titleHTML :: String
    titleHTML = body =~ "<title>[^<]*</title>"

removeMarker ::String -> String
removeMarker = unpack . T.drop (length "<title>") . dropEnd (length "</title>") . pack

chomp :: String -> String
chomp = unpack . strip . pack

escape :: String -> String
escape = fromTagText . head . parseTags

-- Filter an URL so that we don’t make overviews of unknown hosts. Pretty
-- cool to prevent people from going onto sensitive websites.
--
-- All the regex should be put in a file. One per row.
safeHost :: [String] -> String -> Bool
safeHost regexps url = any (url =~) regexps
