module HTML where

import Network.HTTP ( getRequest, getResponseBody, simpleHTTP )
import Text.Regex.Posix ( (=~) )
import Text.Parsec

htmlTitle :: String -> IO (Either String String)
htmlTitle url = do
    r <- simpleHTTP (getRequest prefixedURL)
    either (return . Left) (return . Right . extractTitle) r
  where
    prefixedURL = "http" ++ dropWhile (/=':') url
    
extractTitle :: Response a -> String
extractTitle rq = title
  where
    titleHTML   = getResponseBody rq =~ "<title>.*</title>"
    title       = either noTitle id $ parse titleParser "" titleHTML
    titleParser = between (string "<title>") (string "</title>") (many $ noneOf "\r\n")
    noTitle     = "unknown title!"
