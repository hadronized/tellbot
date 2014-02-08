import Control.Error
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.RWS
import Data.Bifunctor ( second )
import Data.List ( isPrefixOf )
import Data.List.Split ( splitOn )
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
ircNick = "asskel"

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
ircSession = forever $ fromIRC >>= onContent . tail

onContent :: String -> Session ()
onContent c = do
    liftIO (putStrLn c)
    treat
  where
    treat
        | isMsg c   = treatMsg c
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
        -- look for queuing message
        queuing <- gets (maybeToList . M.lookup fromNick)
        --unless ( null queuing ) $ tellBack fromNick queuing
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
    (cmd,arg)  = second tail . break (==' ') $ msg
    unknownCmd = asks conChan >>= \chan -> msgIRC chan $ cmd ++ ", c’est quoi cette connerie encore ?! (help pour la liste, la vraie)"
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
        | length arg > 1 = do
          modify . M.insertWith (++) fromNick $ ["message de " ++ from ++ " : " ++ msg]
          msgIRC chan $ "ok " ++ from ++ ", je le dirai à " ++ fromNick ++ " ;)"
        | otherwise = msgIRC chan "c’est cela oui, on dirait MMyErS qui parle là…"
    (fromNick,msg) = second tail . break (==' ') $ arg

helpCmd :: String -> String -> String -> Session ()
helpCmd _ _ _ = do
    chan <- asks conChan
    msgIRC chan $ "!help         : cette même aide, abruti"
    msgIRC chan $ "!tell dest msg: laisse un message msg à dest"

tellBack :: String -> [String] -> Session ()
tellBack from queuing = do
    --sequence_ . map (msgIRC from) $ queuing
    return ()
