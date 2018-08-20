{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Control.Applicative
import Network.HTTP.Client
import Data.Aeson
import Data.Aeson.Lens
import Text.ParserCombinators.ReadP

import qualified Network.Wreq.Types as WT
import qualified Network.Wreq as W
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Internal as BS (c2w)
import qualified Network.Wreq.Session as S
import qualified Data.Text as T


server_url = "http://localhost:3000/"

data Command = Get { url :: String }
             | Post { url :: String }
             | Print
             | Pop
             | View { path :: String }
             | Set { path :: String, value :: String}
             | Help
             | InvalidCommand
             deriving Show

readHelp :: ReadP Command
readHelp = do
    string "help"
    return Help

readGet:: ReadP Command
readGet = do
    string "get"
    string " "
    url <- munch1 isUrl
    return $ Get url

readPost :: ReadP Command
readPost = do
    string "post"
    string " "
    url <- munch1 isUrl
    return $ Post url

isUrl :: Char -> Bool
isUrl char = char == '/' || (char >= 'a' && char <= 'z') || (char >= '0' && char <= '9')

readPop :: ReadP Command
readPop = do
    string "pop"
    return Pop

readPrint :: ReadP Command
readPrint = do
    string "list"
    return Print

readView :: ReadP Command
readView = do
    string "view"
    string " "
    path <- munch1 isPath
    return $ View path

readSet :: ReadP Command
readSet = do
    string "set"
    string " "
    path <- munch1 isPath
    string " "
    value <- munch1 isValue
    return $ Set path value

isPath :: Char -> Bool
isPath char = (char >= 'a' && char <= 'z') || (char >= '0' && char <= '9')

isValue :: Char -> Bool
isValue char = char == '"' || (char >= 'a' && char <= 'z') || (char >= '0' && char <= '9')

readCommand = readGet <|> readPop <|> readPrint <|> readPost <|> readView <|> readSet <|> readHelp

parseCommand :: ReadP Command -> String -> Command
parseCommand parser input =
    case readP_to_S parser input of
        [] -> InvalidCommand
        ((r, _): _) -> r

data State = State {session :: S.Session, stack :: [LBS.ByteString]}

execute :: Command -> State -> IO State

execute InvalidCommand state = do
    print "Wrong command"
    return state

execute Help state = do
    putStrLn (
        "rest-client\n\n"
     ++ "\tInteractive REST client which fetches json data from " ++ server_url ++ "\n"
     ++ "\tand stores it in a stack for further manipulation (view, set) and to post it back.\n\n"
     ++ "\thead refers to the first element of the stack which can be manipulated or posted\n\n"
     ++ "get <url>\n\tfetch the url and save the response to the head of the stack\n"
     ++ "post <url>\n\tpost the head of the stack to the url\n"
     ++ "list\n\tprint the stack\n"
     ++ "pop\n\tremove the head of the stack\n"
     ++ "view <key>\n\tcreate a new stack element from the value of the key of the stack-head\n"
     ++ "set <key> <json-data>\n\tcreate a new stack-head by changing the value of the current head under the given key")
    return state

execute (Get u) (State session stack) = do
    result <- safeGet session (server_url ++ u)
    case result of
        Right response -> do
            let body = response ^. W.responseBody
            print body
            return $ State session (body:stack)
        Left msg -> do
            print msg
            return $ State session stack

execute (Post u) (State session stack) = do
    result <- safePost session (server_url ++ u) (head stack)
    case result of
        Right response -> do
            let body = response ^. W.responseBody
            print body
            return $ State session (body:stack)
        Left msg -> do
            print msg
            return $ State session stack

execute (View p) (State session stack) = do
    let h = head stack
    case h ^? key (T.pack p) of
        Just l -> do
            let v = encode l
            print v
            return $ State session (v:stack)
        Nothing -> do
            print $ "Invalid key: " ++ p
            return $ State session stack

execute (Set p v) (State session stack) = do
    let h = head stack
    case decode (LBS.pack (map BS.c2w v)) of
        Just v' -> do
            let h' = h & key (T.pack p) .~ v'
            print h'
            return $ State session (h':stack)
        Nothing -> do
            print $ "Invalid key: " ++ p
            return $ State session stack

execute Pop (State session stack) = do
    print $ head stack
    return $ State session (tail stack)

execute Print (State session stack) = do
    print stack
    return $ State session stack

type ResponseResult = Either String (Response LBS.ByteString)

safeGet :: S.Session -> String -> IO ResponseResult
safeGet session url = (Right <$> S.get session url) `E.catch` handler

safePost :: WT.Postable a => S.Session -> String -> a -> IO ResponseResult
safePost session url postData = (Right <$> S.post session url postData) `E.catch` handler

handler :: HttpException -> IO ResponseResult
handler (HttpExceptionRequest _ (StatusCodeException r _)) =
    return $ Left $ BSC.unpack (r ^. W.responseStatus . W.statusMessage)

loop :: State -> IO State
loop (State session stack) = do
    line <- getLine
    let command = parseCommand readCommand line
    newState <- execute command (State session stack)
    loop newState

main :: IO State
main = do
    session <- S.newSession
    let stack = []
    loop $ State session stack
