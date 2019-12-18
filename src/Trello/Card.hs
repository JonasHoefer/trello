{-# LANGUAGE DeriveGeneric #-}

module Trello.Card where

import           Data.Aeson
import           GHC.Generics

data Card = Card { url :: String, name :: String, desc :: String, id :: String } deriving (Generic, Show)

instance FromJSON Card

instance ToJSON Card

