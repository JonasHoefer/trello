{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy          as BS
import           Data.List
import           Data.Maybe
import qualified Data.Map                      as M
import           Data.Functor
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Network.HTTP.Conduit
import           Network.HTTP.Simple

import qualified Trello.Board                  as B
import qualified Trello.List                   as L
import qualified Trello.Card                   as C


data TrelloLogin = TrelloLogin { key :: String, token :: String }

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

allBoards :: TRequest [B.Board]
allBoards =
    getRequest "/1/members/me/boards" $ M.fromList [("fields", "name,url")]

lists :: B.Board -> TRequest [L.List]
lists b = getRequest ("/1/boards/" ++ B.id b ++ "/lists") $ M.fromList
    [ ("cards"      , "none")
    , ("card_fields", "all")
    , ("filter"     , "open")
    , ("fields"     , "all")
    ]

cards :: L.List -> TRequest [C.Card]
cards l = getRequest ("/1/lists/" ++ L.id l ++ "/cards") M.empty

createCard :: L.List -> TRequest C.Card
createCard l = postRequest "/1/cards" $ M.fromList [("idList", L.id l)]

myLogin :: TrelloLogin
myLogin = TrelloLogin
    "6402be28020d54109c32eb97adddc317"
    "48f090faf23781309ae4c4a5d3e82e924db8b6fe5dc11e53f1c13e2c8512468c"

main :: IO ()
main = do
    dat <- runTRequest myLogin $ allBoards <&> head >>= lists <&> head >>= cards
    print dat

