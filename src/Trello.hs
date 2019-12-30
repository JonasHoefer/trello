{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module Trello where

import           Data.Aeson
import qualified Data.ByteString.Lazy          as BS
import           Data.List
import           Data.Maybe
import qualified Data.Map                      as M
import           Data.Functor
import           GHC.Generics
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Network.HTTP.Conduit
import           Network.HTTP.Simple


data TrelloLogin = TrelloLogin { key :: String, token :: String } deriving (Generic, Show, Eq)

instance FromJSON TrelloLogin

instance ToJSON TrelloLogin

newtype TRequest a = TReqeust { unTRequest :: ReaderT TrelloLogin IO a } deriving (Functor, Applicative, Monad, MonadReader TrelloLogin, MonadIO, MonadThrow)

runTRequest l r = (runReaderT . unTRequest $ r) l

api :: String
api = "https://api.trello.com"

buildURL :: String -> M.Map String String -> TRequest String
buildURL path query = do
    TrelloLogin k t <- ask
    let query' = M.fromList [("key", k), ("token", t)] `M.union` query  -- union is left biased
    return
        $   foldl (++) (api ++ path ++ "?")
        $   intersperse "&"
        $   (\(k, v) -> k ++ "=" ++ v)
        <$> M.toList query'

getRequest :: FromJSON a => String -> M.Map String String -> TRequest a
getRequest path query = do
    url <- buildURL path query
    dat <- simpleHttp url
    return . fromJust . decode $ dat

postRequest :: FromJSON a => String -> M.Map String String -> TRequest a
postRequest path query = do
    url      <- buildURL path query
    req      <- parseRequest url
    response <- httpLBS $ req { method = "POST" }
    return . fromJust . decode $ getResponseBody response

