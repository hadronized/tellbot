import Control.Concurrent ( threadDelay )
import Control.Exception ( SomeException, try )
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.RWS
import Data.Bifunctor ( bimap, second )
import Data.Char ( toLower )
import Data.Foldable ( toList )
import Data.List ( intersperse )
import Data.List.Split ( chunksOf, splitOn )
import Data.Foldable ( traverse_ )
import Data.Time.Clock ( getCurrentTime, utctDay )
import Data.Version
import qualified Data.Map as M
import HTML
import Network
import Text.Regex.PCRE ( (=~) )
import System.Environment ( getArgs )
import System.IO

version :: Version
version = Version [0,6,0,2] ["Apfelschorle"]

type Server     = String
type Chan       = String
type Session    = RWST ConInfo () Stories IO

-- IRC connection information.
data ConInfo = ConInfo {
    -- server host
    conHost   :: String
    -- chan
  , conChan   :: String
    -- our nick
  , conNick   :: String
    -- handle
  , conHandle :: Handle
    -- admin password
  , conPwd    :: String
  }

-- A 'Nick' is a case-insensitive 'String'.
newtype Nick = Nick { unNick :: String }

instance Eq Nick where
  Nick a == Nick b = map toLower a == map toLower b

instance Show Nick where
  show (Nick a) = map toLower a

-- For each individual person, keep a list of stories to tell them.
type Stories = M.Map String [String]

-- Port to use to connect to the IRC server.
ircPort :: Int
ircPort = 6667

-- Threshold limit we think we’ll be flooding a channel.
floodThreshold :: Int
floodThreshold = 3

-- If think we’re about to flood, delay messages by this value.
floodDelay :: Int
floodDelay = 500000 -- 500ms

-- Time to reconnect to a server.
reconnectDelay :: Int
reconnectDelay = 1000000 -- 1s

-- Path to the regular expressions to filter HTML titles.
htmlTitleRegPath :: FilePath
htmlTitleRegPath = "./regexps"

-- Send a message to IRC. That message should use the IRC protocol (RFC 1459).
toIRC :: String -> Session ()
toIRC msg = asks conHandle >>= lift . flip hPutStrLn msg

-- Receive a line from IRC. The line is formatted using the IRC protocol (RFC 1459).
fromIRC :: Session String
fromIRC = asks conHandle >>= lift . hGetLine

-- Send a message to someone in the current IRC session.
msgIRC :: String -> String -> Session ()
msgIRC to msg = toIRC $ "PRIVMSG " ++ to ++ " :" ++ msg

-- Notice a message to someone in the current IRC session. Can be a channel as well if the
-- destination starts with a dash ('#').
noticeIRC :: String -> String -> Session ()
noticeIRC to msg = toIRC $ "NOTICE " ++ to ++ " :" ++ msg

-- Parse a list of arguments to retrieve information about the connection.
getConInfo :: [String] -> Either String (Server,Chan,String,String)
getConInfo args
  | length args == 4 = let [host,chan,nick,pwd] = args in Right (host,chan,nick,pwd)
  | otherwise = Left "expected server host, chan, nick and admin password"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  putStrLn . showVersion $ version
  args <- getArgs
  connectIRC args

-- Connect to the IRC server with the given arguments. On a failure, outputs to stderr.
connectIRC :: [String] -> IO ()
connectIRC args = do
    case getConInfo args of
      Right (host,chan,nick,pwd) -> go host chan nick pwd
      Left e -> errLn $ "unable to get connection information: " ++ e
  where
    go host chan nick pwd = do
      putStrLn $ "connecting to " ++ host
      eitherCon <- try $ do
        h <- connectTo host (PortNumber . fromIntegral $ ircPort)
        hSetBuffering h NoBuffering
        session (ConInfo host chan nick h pwd) $ do
          initIRC nick
          joinChan
          idle
      either (reconnect host chan nick pwd) (const $ pure ()) eitherCon
    reconnect host chan nick pwd e = do
      errLn $ "disconnected: " ++ show (e :: SomeException)
      threadDelay reconnectDelay
      go host chan nick pwd

-- Run a new session.
session :: ConInfo -> Session a -> IO a
session cinfo s = do
  (a,_,_) <- runRWST s cinfo M.empty
  return a

-- Initialize the IRC link.
initIRC :: String -> Session ()
initIRC nick = do
  toIRC "USER a b c :d"
  toIRC ("NICK " ++ nick)

-- Join the channel.
joinChan :: Session ()
joinChan = do
  chan <- asks conChan
  liftIO . putStrLn $ "joining " ++ chan
  toIRC ("JOIN " ++ chan)

-- Idle and wait for activity.
idle :: Session ()
idle = forever $ fromIRC >>= onIRCActivity . purgeContent

-- Purge the content of the message given by IRC by removing newlines.
purgeContent :: String -> String
purgeContent = filter $ \c -> not $ c `elem` "\n\r"

-- Reactive function called whenever a new IRC activity has been detected.
onIRCActivity :: String -> Session ()
onIRCActivity c = do
    liftIO (putStrLn c)
    dispatch
  where
    tailC = tail c
    dispatch
        | isMsg c   = treatMsg tailC
        | isKick c  = treatKick c
        | isPing c  = treatPing c
        | otherwise = pure ()

-- FIXME: those functions are not really safe and might be flaws
-- Is a message a ping?
isPing :: String -> Bool
isPing c = "PING" `elem` words c

-- Is a message a user message?
isMsg :: String -> Bool
isMsg c = "PRIVMSG" `elem` words c

-- Is a message a kick?
isKick :: String -> Bool
isKick c = "KICK" `elem` words c

-- Respond to ping.
treatPing :: String -> Session ()
treatPing ping = toIRC pong
  where
    pong = "PONG" ++ numericPing
    numericPing = snd . break (==' ') $ ping

-- When a user writes a message, we need to do several things.
--
-- In the first place, we want to filter out the case when we are the one to talk. In that case, we
-- just do nothing.
--
-- Then, if it’s someone else, we just try to tell them stories. Then, we look for a URL in their
-- message. If we’ve found a URL, we just try to extract its HTML title, and broadcast it on the
-- channel. If we don’t find a URL or we fail to parse the title, we just do nothing.
--
-- Finally, we check whether the message is not a command message. If so, we just branch on the
-- 'onCmd' function.
treatMsg :: String -> Session ()
treatMsg msg = do
    nick <- asks conNick
    chan <- asks conChan
    unless (null content || Nick emitter == Nick nick) $ do
      tellStories emitter
      let url = extractUrl content
      unless (null url) $ do
        title <- liftIO (htmlTitle htmlTitleRegPath url)
        traverse_ (\t -> msgIRC chan t) title
      when (head content == '!') $ do
        onCmd emitter recipient (tail content)
  where
    (emitter,recipient,content) = emitterRecipientContent msg

-- Extract the emitter, the recipient and the message.
emitterRecipientContent :: String -> (String,String,String)
emitterRecipientContent msg = (emitter,recipient,content)
  where
    (from':_:recipient:content') = splitOn " " msg
    emitter = fst . break (=='!') $ from'
    content = tailSafe (unwords content')

-- Extract the URL out of a message. If no URL is found, gives an empty string.
extractUrl :: String -> String
extractUrl = (=~ "https?://[^ ]+")

-- We might want to know whether we got kicked. If so, we just rejoin the server and insult the
-- person who has kicked us! 
treatKick :: String -> Session ()
treatKick msg = do
    nick <- asks conNick
    chan <- asks conChan
    when (kicked == nick) $ do
      joinChan
      msgIRC chan $ emitter ++ ": you sonavabitch."
  where
    (from',_,content) = emitterRecipientContent msg
    emitter = tailSafe from'
    kicked = tailSafe $ dropWhile (/=':') content

-- When someone tries to enter a command, we need to validate the command. First, the command is
-- looked up. If it doesn’t exist, nothing is performed, because it could be someone trying to
-- bruteforce us.
onCmd :: String -> String -> String -> Session ()
onCmd emitter recipient msg = traverse_ treatCmd (M.lookup cmd commands)
  where
    (cmd,arg) = second tailSafe . break (==' ') $ msg
    treatCmd c = c emitter recipient arg

-- List of available commands.
commands :: M.Map String (String -> String -> String -> Session ())
commands = M.fromList
    [
      ("tell",tellCmd)
    , ("do",doCmd)
    , ("help",helpCmd)
    ]

-- Function associated with the "tell" command.
tellCmd :: String -> String -> String -> Session ()
tellCmd emitter _ arg = do
    chan <- asks conChan
    treat chan
  where
    treat chan
        | length arg > 1 && not (null msg) = do
          nick <- fmap Nick (asks conNick)
          if emitterNick == nick
          then
            msgIRC chan "I'll tell myself for sure pal!"
          else do
            now <- liftIO $ utctDay `liftM` getCurrentTime
            modify . M.insertWith (flip (++)) (show emitterNick) $
              [show now ++ ", \STX\ETX6" ++ emitter ++ "\SI told \STX\ETX2" ++ (unNick emitterNick) ++ "\SI: " ++ msg]
            msgIRC emitter "\\_o<"
        | otherwise = msgIRC chan "nope!"
    (emitterNick,msg) = bimap Nick tailSafe . break (==' ') $ arg

-- Function associated with the "do" command. That command is used to perform several administration
-- tasks by making the bot *do* things for us.
doCmd :: String -> String -> String -> Session ()
doCmd emitter recipient arg = do
    chan   <- asks conChan
    myNick <- asks conNick
    pwd    <- asks conPwd
    treatDo chan myNick pwd
  where
    treatDo chan myNick pwd
        | recipient == chan = msgIRC emitter "I'm sorry, I feel naked in public ;)"
        | Nick recipient == Nick myNick && length args >= 3 = executeDo chan pwd
        | otherwise = msgIRC emitter "huhu, something went terribly wrong!"
    args = words arg
    userPwd:action:actionParams = args
    executeDo chan pwd
        | pwd /= userPwd = msgIRC emitter "wrong password!"
        | otherwise = executeAction chan
    executeAction chan
        | action == "op"     = traverse_ (toIRC . (mode chan "+o"++)) actionParams
        | action == "deop"   = traverse_ (toIRC . (mode chan "-o"++)) actionParams
        | action == "say"    = msgIRC chan (unwords actionParams)
        | action == "kick"   = traverse_ (toIRC . (("KICK " ++ chan ++ " ")++)) actionParams
        | action == "notice" = noticeIRC chan (unwords actionParams)
        | otherwise = msgIRC emitter "unknown action"
    mode chan m = "MODE " ++ chan ++ " " ++ m ++ " "

-- Display the help to the recipient.
helpCmd :: String -> String -> String -> Session ()
helpCmd recipient _ _ = do
  myNick <- asks conNick
  msgIRC recipient $ "!tell dest msg: leave a message to a beloved"
  msgIRC recipient $ "!do pwd action params: perform an action"
  msgIRC recipient $ "-   -   op user0 user1...: grant op privileges"
  msgIRC recipient $ "-   -   deop user0 user1...: revoke op privileges"
  msgIRC recipient $ "-   -   kick user0 user1...: kick them all!"
  msgIRC recipient $ "-   -   say blabla: make " ++ myNick ++ " say something"
  msgIRC recipient $ "-   -   notice msg: notice the channel something"
  msgIRC recipient . showVersion $ version
  msgIRC recipient $ "written in Haskell by phaazon"

-- Tell stories to someone.
tellStories :: String -> Session ()
tellStories recipient = do
    stories <- gets (toList . M.lookup (show $ Nick recipient))
    let
      cstories = concat stories
      chunks = map (mapM_ $ msgIRC recipient) . chunksOf floodThreshold $ cstories
      tells  = intersperse (liftIO $ threadDelay floodDelay) chunks
    unless (null stories) $ do
      sequence_ tells
      modify (M.delete . show $ Nick recipient)

errLn :: (MonadIO m) => String -> m ()
errLn = liftIO . hPutStrLn stderr

-- Safer tail.
tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe (_:xs) = xs
