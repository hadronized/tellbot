module HTML where

import Control.Exception ( SomeException, catch )
import Data.ByteString.Lazy ( toStrict )
import Data.Char
import Network.HTTP.Conduit
import Data.Text as T ( concat, lines, pack, strip, unpack )
import Data.Text.Encoding ( decodeUtf8 )
import Text.HTML.TagSoup
import Text.Regex.PCRE ( (=~) )

htmlTitle :: FilePath -> String -> IO (Maybe String)
htmlTitle regPath url = flip catch handleException $ do
    regexps <- fmap Prelude.lines $ readFile regPath 
    if (safeHost regexps url) then do
      putStrLn $ url ++ " is safe"
      fmap (extractTitle . unpack . T.concat . T.lines . decodeUtf8 . toStrict) (simpleHttp url)
      else
        pure Nothing
  where
    handleException :: SomeException -> IO (Maybe String)
    handleException _ = pure mempty
    
extractTitle :: String -> Maybe String
extractTitle body =
  case dropTillTitle (parseTags body) of
    (TagText title:TagClose "title":_) -> pure ("\ETX7«\ETX6 " ++ chomp title ++ " \ETX7»\SI")
    _ -> Nothing

dropTillTitle :: [Tag String] -> [Tag String]
dropTillTitle [] = []
dropTillTitle (TagOpen "title" _ : xs) = xs
dropTillTitle (_:xs) = dropTillTitle xs

chomp :: String -> String
chomp = filter (\c -> ord c >= 32 && (isAlphaNum c || isPunctuation c || c == ' ')) . unpack . strip . pack

-- Filter an URL so that we don’t make overviews of unknown hosts. Pretty
-- cool to prevent people from going onto sensitive websites.
--
-- All the regex should be put in a file. One per row.
safeHost :: [String] -> String -> Bool
safeHost regexps url = any (url =~) regexps
