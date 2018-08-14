{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Control.Applicative
import Network.HTTP.Client
import Data.Aeson.Lens (key, _String, _Object)
import Text.ParserCombinators.ReadP

import qualified Network.Wreq as W
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wreq.Session as S
import qualified Data.Text as T


server_url = "http://localhost:3000/"

data Command = Get { url :: String }
             | Pop
             deriving Show

readGet:: ReadP Command
readGet = do
    string "get"
    string " "
    url <- munch1 (\char -> char >= 'a' && char <= 'z')
    return $ Get url

readPop :: ReadP Command
readPop = do
    string "pop"
    return Pop

readCommand = readGet <|> readPop

type CommandResult = Either String Command

parseMaybe :: ReadP Command -> String -> CommandResult
parseMaybe parser input =
    case readP_to_S parser input of
        [] -> Left "Wrong command"
        ((r, _): _) -> Right r

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
        Right (Pop) -> do
            print $ head stack
            loop session (tail stack)


type ResponseResult = Either String (Response LBS.ByteString)

safeGet :: S.Session -> String -> IO ResponseResult
safeGet session url = (Right <$> S.get session url) `E.catch` handler

handler :: HttpException -> IO ResponseResult
handler (HttpExceptionRequest _ (StatusCodeException r _)) = return $ Left $ BSC.unpack (r ^. W.responseStatus . W.statusMessage)


main :: IO ()
main = do
    session <- S.newSession
    let stack = []
    loop session stack
