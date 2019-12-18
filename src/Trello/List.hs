{-# LANGUAGE DeriveGeneric #-}

module Trello.List where

import           Data.Aeson
import           GHC.Generics

data List = List { id :: String, name :: String, closed :: Bool, idBoard :: String, pos :: Float, subscribed :: Bool, softLimit :: Maybe Integer } deriving (Generic, Show)

instance FromJSON List
