{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Monoid ((<>))
import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)
import Web.Scotty

data User =  User { userId :: Int, userName :: String }
           | NoUser
           deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

bob :: User
bob = User { userId = 1, userName = "bob" }

jenny :: User
jenny = User { userId = 2, userName = "jenny" }

allUsers :: [User]
allUsers = [bob, jenny]

routes :: ScottyM ()
routes = do
    get "/hello" hello
    get "/hello/:name" $ do
        name <- param "name"
        text ("hello " <> name <> "!")
    get "/users" $ do
        json allUsers
    get "/users/:id" $ do
        id <- param "id"
        json (filter (matchesId id) allUsers)
    post "/users" $ do
        d <- jsonData
        text $ case d of
            User id name -> "hey"
            NoUser -> "nope"

matchesId :: Int -> User -> Bool
matchesId id user = userId user == id

hello :: ActionM ()
hello = text "hello world!"

main = do
    putStrLn "Starting Server..."
    scotty 3000 routes
