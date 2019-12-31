{-# LANGUAGE DeriveGeneric #-}

module Trello.Attachment where

import           Prelude                 hiding ( id )

import           Data.Aeson
import           Data.Maybe
import qualified Data.Map                      as M
import           GHC.Generics

import           Trello
import           Trello.Util
import qualified Trello.Card                   as C


data Attachment = Attachment { id :: String, bytes :: Integer, name :: String } deriving (Generic, Show)

instance FromJSON Attachment

instance ToJSON Attachment

all :: C.Card -> TRequest [Attachment]
all c = getRequest ("/1/cards/" ++ C.id c ++ "/attachments") M.empty

post :: C.Card -> String -> TRequest Attachment
post c url = postRequest ("/1/cards/" ++ C.id c ++ "/attachments") $ M.singleton "url" url
