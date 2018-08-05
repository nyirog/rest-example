{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main (main) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Reader

import Data.Default.Class
import Data.String
import Data.Text.Lazy (Text)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, (.=), withObject, (.:))
import Data.Map
import Data.Monoid ((<>))

import Network.Wai.Middleware.RequestLogger

import Prelude

import Web.Scotty.Trans
import Network.HTTP.Types.Status

data User =  User { userName :: String }
           | NoUser
           deriving Show

instance ToJSON User where
    toJSON (User userName) = object ["name" .= toJSON userName]
    toJSON NoUser = Null

instance FromJSON User where
    parseJSON = withObject "User" $ \o -> do
        name_ <- o .: "name"
        return (User name_)

newtype Users = Users { users :: Map Int User }

instance Default Users where
    def = Users $ fromList [(0, (User "admin"))]

addUser :: User -> Users -> Users
addUser u us = Users $ Data.Map.insert (nextId us) u (users us)
    where
        nextId us = length $ users us

newtype WebM a = WebM { runWebM :: ReaderT (TVar Users) IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar Users))

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

gets :: (Users -> b) -> WebM b
gets f = ask >>= liftIO . readTVarIO >>= return . f

modify :: (Users -> Users) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

main :: IO ()
main = do
    sync <- newTVarIO def
    let runActionToIO m = runReaderT (runWebM m) sync

    scottyT 3000 runActionToIO app

app :: ScottyT Text WebM ()
app = do
    middleware logStdoutDev
    get "/users" $ do
        users <- webM $ gets users
        json $ users
    get "/users/:id" $ do
        id <- param "id"
        users <- webM $ gets users
        case (Data.Map.lookup id users) of
            Just user -> json user
            Nothing -> status notFound404
    post "/users" $ do
        d <- jsonData
        case d of
            user@(User name) -> do
                webM $ modify $ addUser user
                status created201
            NoUser -> status badRequest400
