import Control.Concurrent ( threadDelay )
import Control.Exception ( SomeException, try )
import Control.Error
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.RWS
import Data.Bifunctor ( second )
import Data.List ( intersperse, isPrefixOf )
import Data.List.Split ( chunksOf, splitOn )
import Data.Time.Clock ( getCurrentTime, utctDay )
import Data.Version
import qualified Data.Map as M
import Network
import System.Environment ( getArgs )
import System.IO

version :: Version
version = Version [0,3,0,0] ["un","ptit","pot?!"]

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

floodThreshold :: Int
floodThreshold = 3

floodDelay :: Int
floodDelay = 500000 -- 500ms

reconnectDelay :: Int
reconnectDelay = 1000000 -- 1s

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
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    
    putStrLn . showVersion $ version
    args <- getArgs
    runFailableIO (start args) >>= either errLn return

getChan :: [String] -> Failable (Server,Chan,String)
getChan args = do
    unless ( length args == 3 ) . left $ "expected server host, chan and nick"
    let [host,chan,nick] = args
    return (host,chan,nick)

start :: [String] -> FailableIO ()
start args = do
    (serv,chan,nick) <- hoistEither . runFailable $ getChan args
    liftIO . withSocketsDo $ connectIRC serv chan nick

connectIRC :: Server -> Chan -> String -> IO ()
connectIRC serv chan nick = do
      putStrLn $ "connecting to " ++ serv
      eitherCon <- try $ do
        h <- connectTo serv (PortNumber . fromIntegral $ ircPort)
        hSetBuffering h NoBuffering
        session (ConInfo serv chan nick h) $ do
          initIRC nick
          openChan
          ircSession       
      either reconnect (const $ return ()) eitherCon
  where
    reconnect :: SomeException -> IO ()
    reconnect e =
        err (show e) >> threadDelay reconnectDelay >> connectIRC serv chan nick

initIRC :: String -> Session ()
initIRC nick = do
    toIRC "USER a b c :d"
    toIRC $ "NICK " ++ nick

openChan :: Session ()
openChan = do
    chan <- asks conChan
    liftIO . putStrLn $ "joining " ++ chan
    joinChan

joinChan :: Session ()
joinChan = asks conChan >>= toIRC . ("JOIN "++)

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
    tailC = tail c
    treat
        | isMsg c   = treatMsg tailC
        | isJoin c  = treatJoin tailC
        | isKick c  = treatKick c
        | isPing c  = treatPing c
        | otherwise = return ()

-- FIXME: those functions are not really safe and might be flaws
isPing :: String -> Bool
isPing c = "PING" `elem` words c

isMsg :: String -> Bool
isMsg c = "PRIVMSG" `elem` words c

isJoin :: String -> Bool
isJoin c = "JOIN" `elem` words c

isKick :: String -> Bool
isKick c = "KICK" `elem` words c

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
    liftIO . putStrLn $ "from: " ++ fromNick ++ ", to: " ++ to ++ ": " ++ content
    unless ( null content || fromNick == nick ) $ do
      tellStories fromNick
      when ( head content == '!') $ do
        onCmd fromNick to (tail content)
  where
    (fromNick,to,content) = emitterRecipientContent msg

treatJoin :: String -> Session ()
treatJoin msg = do
    return ()
{-
    nick <- asks conNick
    unless (from == nick) $ tellStories from
  where
    (from,to,_) = emitterRecipientContent msg
-}

treatKick :: String -> Session ()
treatKick msg = do
    nick <- asks conNick
    chan <- asks conChan
    when (kicked == nick) $ do
      liftIO . putStrLn $ "woah, I was kicked by " ++ from
      joinChan
      msgIRC chan $ from ++ ": you sonavabitch."
  where
    (from',to,content) = emitterRecipientContent msg
    from               = tailSafe from'
    kicked             = tailSafe $ dropWhile (/=':') content
      
-- Extract the emitter, the recipient and the message.
emitterRecipientContent :: String -> (String,String,String)
emitterRecipientContent msg = (from,to,content)
  where
    (from':_:to:content') = splitOn " " msg
    from                  = fst . break (=='!') $ from'
    content               = tailSafe (unwords content')

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
          nick <- asks conNick
          if fromNick == nick then
            msgIRC chan "I'll tell myself for sure pal!"
            else do
              userPresent <- do
                chan <- asks conChan
                toIRC $ "NAMES " ++ chan
                names <- (filter $ \c -> not $ c `elem` "?@!#:") `liftM` fromIRC
                liftIO . putStrLn $ "names: " ++ names
                return (fromNick `elem` words names)
              if userPresent then
                msgIRC chan "that folk is just there you idiot."
                else do
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
    msgIRC chan . showVersion $ version
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
