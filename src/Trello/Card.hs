{-# LANGUAGE DeriveGeneric, TupleSections #-}

module Trello.Card where

import           Prelude                 hiding ( id )

import           Data.Aeson
import           Data.Maybe
import qualified Data.Map                      as M
import           GHC.Generics

import           Trello
import           Trello.Util
import qualified Trello.List                   as L


data Card = Card { url :: String, closed :: Bool, name :: String, desc :: String, idAttachmentCover :: Maybe String, idBoard :: String, idList :: String, id :: String } deriving (Generic, Show)

instance FromJSON Card

instance ToJSON Card

all :: L.List -> TRequest [Card]
all l = getRequest ("/1/lists/" ++ L.id l ++ "/cards") M.empty

post :: L.List -> TRequest Card
post l = postRequest "/1/cards" $ M.fromList [("idList", L.id l)]

delete :: Card -> TRequest ()
delete c = deleteRequest ("/1/cards/" ++ id c) M.empty

put :: Card -> TRequest ()
put c = putRequest ("/1/cards/" ++ id c) $ toQuery . fromJust . decode . encode $ c

label :: Card -> String -> Maybe String -> TRequest ()
label c color name = postRequest ("/1/cards/" ++ id c ++ "/labels") $ M.fromList $ ins (("name", ) <$> name) [("color", color)]
    where ins mx xs = maybe xs (:xs) mx

markAssociatedNotificationsRead :: Card -> TRequest ()
markAssociatedNotificationsRead c = postRequest ("/cards/" ++ id c ++ "/markAssociatedNotificationsRead") M.empty




