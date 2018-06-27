{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Map
import Data.Monoid ((<>))
import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)
import Web.Scotty
import Network.HTTP.Types.Status

data User =  User { userName :: String }
           | NoUser
           deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

bob :: User
bob = User { userName = "bob" }

jenny :: User
jenny = User { userName = "jenny" }

allUsers :: Map Int User
allUsers = fromList [(1, bob), (2, jenny)]

routes :: ScottyM ()
routes = do
    get "/users" $ do
        json allUsers
    get "/users/:id" $ do
        id <- param "id"
        case (Data.Map.lookup id allUsers) of
            Just u -> json u
            Nothing -> status notFound404
    post "/users" $ do
        d <- jsonData
        text $ case d of
            User name -> "hey"
            NoUser -> "nope"

main = do
    putStrLn "Starting Server..."
    scotty 3000 routes
