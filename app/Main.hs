module Main where

import qualified Data.Map                      as M
import           Data.Functor

import           Trello
import qualified Trello.Board                  as B
import qualified Trello.List                   as L
import qualified Trello.Card                   as C

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

