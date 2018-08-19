{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Control.Applicative
import Network.HTTP.Client
import Data.Aeson
import Data.Aeson.Lens
import Text.ParserCombinators.ReadP
import GHC.Word

import qualified Network.Wreq.Types as WT
import qualified Network.Wreq as W
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import qualified Network.Wreq.Session as S
import qualified Data.Text as T


server_url = "http://localhost:3000/"

data Command = Get { url :: String }
             | Post { url :: String }
             | Print
             | Pop
             | View { path :: String }
             | Set { path :: String, value :: String}
             deriving Show

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

readCommand = readGet <|> readPop <|> readPrint <|> readPost <|> readView <|> readSet

type CommandResult = Either String Command

parseMaybe :: ReadP Command -> String -> CommandResult
parseMaybe parser input =
    case readP_to_S parser input of
        [] -> Left "Wrong command"
        ((r, _): _) -> Right r

loop :: S.Session -> [LBS.ByteString] -> IO ()
loop session stack = do
    line <- getLine
    case parseMaybe readCommand line of
        Left msg -> do
            print msg
            loop session stack
        Right (Get u) -> do
            result <- safeGet session (server_url ++ u)
            case result of
                Right response -> do
                    let body = response ^. W.responseBody
                    print body
                    loop session (body:stack)
                Left msg -> do
                    print msg
                    loop session stack
        Right (Post u) -> do
            result <- safePost session (server_url ++ u) (head stack)
            case result of
                Right response -> do
                    let body = response ^. W.responseBody
                    print body
                    loop session (body:stack)
                Left msg -> do
                    print msg
                    loop session stack
        Right (View p) -> do
            let h = head stack
            case h ^? key (T.pack p) of
                Just l -> do
                    let v = encode l
                    print v
                    loop session (v:stack)
                Nothing -> do
                    print $ "Invalid key: " ++ p
                    loop session stack
        Right (Set p v) -> do
            let h = head stack
            case decode (LBS.pack (map BS.c2w v)) of
                Just v' -> do
                    let h' = h & key (T.pack p) .~ v'
                    print h'
                    loop session (h':stack)
                Nothing -> do
                    print $ "Invalid key: " ++ p
                    loop session stack
        Right (Pop) -> do
            print $ head stack
            loop session (tail stack)
        Right (Print) -> do
            print stack
            loop session stack


type ResponseResult = Either String (Response LBS.ByteString)

safeGet :: S.Session -> String -> IO ResponseResult
safeGet session url = (Right <$> S.get session url) `E.catch` handler

safePost :: WT.Postable a => S.Session -> String -> a -> IO ResponseResult
safePost session url postData = (Right <$> S.post session url postData) `E.catch` handler

handler :: HttpException -> IO ResponseResult
handler (HttpExceptionRequest _ (StatusCodeException r _)) = return $ Left $ BSC.unpack (r ^. W.responseStatus . W.statusMessage)


main :: IO ()
main = do
    session <- S.newSession
    let stack = []
    loop session stack
