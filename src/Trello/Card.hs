{-# LANGUAGE DeriveGeneric #-}

module Trello.Card where

import           Data.Aeson
import qualified Data.Map                      as M
import           GHC.Generics

import           Trello
import qualified Trello.List                   as L


data Card = Card { url :: String, name :: String, desc :: String, id :: String } deriving (Generic, Show)

instance FromJSON Card

instance ToJSON Card

all :: L.List -> TRequest [Card]
all l = getRequest ("/1/lists/" ++ L.id l ++ "/cards") M.empty

create :: L.List -> TRequest Card
create l = postRequest "/1/cards" $ M.fromList [("idList", L.id l)]

