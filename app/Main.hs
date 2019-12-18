{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Main where

import           Data.Aeson
import           Data.ByteString.Lazy
import           Data.Maybe
import           Data.Functor
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Network.HTTP.Conduit           ( simpleHttp )

import qualified Trello.Board                   as B
import qualified Trello.List                    as L
import qualified Trello.Card                    as C


data TrelloLogin = TrelloLogin { key :: String, token :: String }

newtype Request a = Reqeust { unRequest :: ReaderT TrelloLogin IO a } deriving (Functor, Applicative, Monad, MonadReader TrelloLogin, MonadIO)

runRequest l r = (runReaderT . unRequest $ r) l 

allBoards :: Request [B.Board]
allBoards = do 
  TrelloLogin k t <- ask
  dat <- simpleHttp $ "https://api.trello.com/1/members/me/boards?fields=name,url&key=" ++ k ++ "&token=" ++ t
  return . fromJust . decode $ dat -- force unwrap to keep it simple

lists :: B.Board -> Request [L.List]
lists b = do
  TrelloLogin k t <- ask
  dat <- simpleHttp $ "https://api.trello.com/1/boards/" ++ B.id b ++ "/lists?cards=none&card_fields=all&filter=open&fields=all&key=" ++ k ++ "&token=" ++ t
  return . fromJust . decode $ dat

cards :: L.List -> Request [C.Card]
cards l = do
  TrelloLogin k t <- ask
  dat <- simpleHttp $   "https://api.trello.com/1/lists/" ++ L.id l ++ "/cards?key=" ++ k ++ "&token=" ++ t
  return . fromJust . decode $ dat
  
myLogin :: TrelloLogin
myLogin = TrelloLogin "6402be28020d54109c32eb97adddc317" "48f090faf23781309ae4c4a5d3e82e924db8b6fe5dc11e53f1c13e2c8512468c"

main :: IO ()
main = do 
  dat <- runRequest myLogin $ allBoards <&> Prelude.head >>= lists <&> Prelude.head >>= cards
  print dat
 