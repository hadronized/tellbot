module HTML where

import Control.Exception ( SomeException, catch, evaluate )
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
      if url =~ "https?://www\\.youtube\\.com"
      then do
        resp <- simpleHttp ("http://www.getlinkinfo.com/info?link=" ++ url)
        evaluate $ extractTitle True . unpack . T.concat . T.lines . decodeUtf8 $ toStrict resp
      else do
        resp <- simpleHttp url
        evaluate $ extractTitle False . unpack . T.concat . T.lines . decodeUtf8 $ toStrict resp
      else
        pure Nothing
  where
    handleException :: SomeException -> IO (Maybe String)
    handleException e = do
      putStrLn $ "Exception: " ++ show e
      pure Nothing
    
extractTitle :: Bool -> String -> Maybe String
extractTitle True body = case dropTillTitle True (parseTags body) of
  (TagText title:TagClose "b":TagClose "dd":_) -> pure ("\ETX7«\ETX6 " ++ chomp title ++ " \ETX7»\SI")
  _ -> Nothing
extractTitle False body = case dropTillTitle False (parseTags body) of
  (TagText title:TagClose "title":_) -> pure ("\ETX7«\ETX6 " ++ chomp title ++ " \ETX7»\SI")
  _ -> Nothing

dropTillTitle :: Bool -> [Tag String] -> [Tag String]
dropTillTitle _ [] = []
dropTillTitle True (TagOpen "dt" [("class","link-title")]:TagText _:TagClose "dt":TagText _:TagOpen "dd" []:TagOpen "b" []:xs) = xs
dropTillTitle False (TagOpen "title" _:xs) = xs
dropTillTitle b (_:xs) = dropTillTitle b xs

chomp :: String -> String
chomp = filter (\c -> ord c >= 32 && (isAlphaNum c || isPunctuation c || c == ' ')) . unpack . strip . pack

-- Filter an URL so that we don’t make overviews of unknown hosts. Pretty
-- cool to prevent people from going onto sensitive websites.
--
-- All the regex should be put in a file. One per row.
safeHost :: [String] -> String -> Bool
safeHost regexps url = any (url =~) regexps
