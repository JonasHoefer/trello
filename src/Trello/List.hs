{-# LANGUAGE DeriveGeneric #-}

module Trello.List where

import qualified Data.Map                      as M
import           Data.Aeson
import           GHC.Generics

import           Trello
import qualified Trello.Board                  as B


data List = List { id :: String, name :: String, closed :: Bool, idBoard :: String, pos :: Float, subscribed :: Bool, softLimit :: Maybe Integer } deriving (Generic, Show)

instance FromJSON List

all :: B.Board -> TRequest [List]
all b = getRequest ("/1/boards/" ++ B.id b ++ "/lists") $ M.fromList
    [ ("cards"      , "none")
    , ("card_fields", "all")
    , ("filter"     , "open")
    , ("fields"     , "all")
    ]


