{-# LANGUAGE DeriveGeneric #-}

module Trello.Board where

import           Data.Aeson
import           GHC.Generics

data Board = Board { url :: String, name :: String, id :: String } deriving (Generic, Show)

instance FromJSON Board
