import Control.Concurrent ( threadDelay )
import Control.Error
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.RWS
import Data.Bifunctor ( second )
import Data.List ( intersperse, isPrefixOf )
import Data.List.Split ( chunksOf, splitOn )
import Data.Time.Clock ( getCurrentTime, utctDay )
import qualified Data.Map as M
import Network
import System.Environment ( getArgs )
import System.IO

type Failable   = EitherT String Identity
type FailableIO = EitherT String IO
type Server     = String
type Chan       = String
type Session    = RWST ConInfo () Stories IO

data ConInfo = ConInfo {
    -- server host
    conHost :: String
    -- chan
  , conChan :: String
    -- our nick
  , conNick :: String
    -- handle
  , conHandle :: Handle
  }

-- for each individual dudes, keep a list of stories to tell
type Stories = M.Map String [String]

ircPort :: Int
ircPort = 6667

ircNick :: String
ircNick = "kwak"

floodThreshold :: Int
floodThreshold = 3

floodDelay :: Int
floodDelay = 500000

session :: ConInfo -> Session a -> IO a
session cinfo s = do
    (a,_,_) <- runRWST s cinfo M.empty
    return a

toIRC :: String -> Session ()
toIRC msg = asks conHandle >>= lift . flip hPutStrLn msg

fromIRC :: Session String
fromIRC = asks conHandle >>= lift . hGetLine

msgIRC :: String -> String -> Session ()
msgIRC to msg = toIRC $ "PRIVMSG " ++ to ++ " :" ++ msg
    
runFailable = runIdentity . runEitherT
runFailableIO = runEitherT

main :: IO ()
main = do
    args <- getArgs
    runFailableIO (start args) >>= either errLn return

getChan :: [String] -> Failable (Server,Chan)
getChan args = do
    unless ( length args == 2 ) . left $ "expected server host and chan"
    let [host,chan] = args
    return (host,chan)

start :: [String] -> FailableIO ()
start args = do
    (serv,chan) <- hoistEither . runFailable $ getChan args
    liftIO . withSocketsDo $ do
      putStrLn $ "connecting to " ++ serv
      h <- connectTo serv (PortNumber . fromIntegral $ ircPort)
      hSetBuffering stdout NoBuffering
      hSetBuffering stderr NoBuffering
      hSetBuffering h NoBuffering

      session (ConInfo serv chan nick h) $ do
        initIRC
        openChan
        ircSession
  where
    nick = ircNick

initIRC :: Session ()
initIRC = do
    toIRC "USER a b c :d"
    toIRC $ "NICK " ++ ircNick

openChan :: Session ()
openChan = do
    chan <- asks conChan
    liftIO . putStrLn $ "joining " ++ chan
    toIRC $ "JOIN " ++ chan

quitChan :: Session ()
quitChan = toIRC "QUIT"

ircSession :: Session ()
ircSession = forever $ fromIRC >>= onContent . purgeContent

purgeContent :: String -> String
purgeContent = filter (\c -> not $ c `elem` "\n\r")

onContent :: String -> Session ()
onContent c = do
    liftIO (putStrLn c)
    treat
  where
    treat
        | isMsg c   = treatMsg (tail c)
        | isPing c  = treatPing c
        | otherwise = return ()

isPing :: String -> Bool
isPing c = "PING" `elem` words c

isMsg :: String -> Bool
isMsg c = "PRIVMSG" `elem` words c

treatPing :: String -> Session ()
treatPing ping = do
    toIRC pong
  where
    pong        = "PONG" ++ numericPing
    numericPing = snd . break (==' ') $ ping

treatMsg :: String -> Session ()
treatMsg msg = do
    chan <- asks conChan
    nick <- asks conNick
    liftIO . putStrLn $ "from: " ++ fromNick ++ ", to: " ++ to ++ ": " ++ msgContent
    unless ( null content || from == nick ) $ do
        tellStories fromNick
        when ( head msgContent == '!') $ do
          onCmd fromNick to (tail msgContent)
  where
    (from:_:to:content) = splitOn " " msg
    msgContent = tail $ unwords content
    fromNick = fst . break (=='!') $ from

onCmd :: String -> String -> String -> Session ()
onCmd from to msg = do
    liftIO . putStrLn $ "searching command " ++ cmd ++ ": " ++ show found
    maybe unknownCmd treatCmd (M.lookup cmd commands)
  where
    (cmd,arg)  = second tailSafe . break (==' ') $ msg
    unknownCmd = return ()
    treatCmd c = c from to arg
    found = cmd `M.member` commands

commands :: M.Map String (String -> String -> String -> Session ())
commands = M.fromList
    [
      ("tell",tellCmd)
    , ("help",helpCmd)
    ]

tellCmd :: String -> String -> String -> Session ()
tellCmd from to arg = do
    chan <- asks conChan
    treat chan
  where
    treat chan
        | length arg > 1 && not (null msg) = do
          now <- liftIO $ utctDay `liftM` getCurrentTime
          modify . M.insertWith (flip (++)) fromNick $
            [show now ++ ", " ++ from ++ " told " ++ fromNick ++ ": " ++ msg]
          msgIRC chan "\\_o<"
        | otherwise = msgIRC chan "nope!"
    (fromNick,msg) = second tailSafe . break (==' ') $ arg

helpCmd :: String -> String -> String -> Session ()
helpCmd _ _ _ = do
    chan <- asks conChan
    msgIRC chan $ "!help         : this help, you dumb"
    msgIRC chan $ "!tell dest msg: leave a message to a beloved"
    msgIRC chan $ "written in Haskell (ahah!) by skypers with a lot of luv <3"

-- FIXME: host & ident
tellStories :: String -> Session ()
tellStories nick = do
    chan    <- asks conChan
    stories <- gets (maybeToList . M.lookup nick)
    let
      cstories = concat stories
      chunks = map (mapM_ $ msgIRC chan) . chunksOf floodThreshold $ cstories
      tells  = intersperse (liftIO $ threadDelay floodDelay) chunks
    unless (null stories) $ do
      sequence_ tells
      modify (M.delete nick)
