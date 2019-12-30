{-# LANGUAGE DeriveGeneric #-}

module Trello.Board where

import           Data.Aeson
import qualified Data.Map                      as M
import           GHC.Generics

import           Trello

data Board = Board { url :: String, name :: String, id :: String } deriving (Generic, Show)

instance FromJSON Board

all :: TRequest [Board]
all = getRequest "/1/members/me/boards" $ M.fromList [("fields", "name,url")]

