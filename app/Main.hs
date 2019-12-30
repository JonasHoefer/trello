module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Functor

import           Trello
import qualified Trello.Board                  as B
import qualified Trello.List                   as L
import qualified Trello.Card                   as C


main :: IO ()
main = do
    login <- fromJust . decode <$> BS.readFile "login.json"
    dat   <- runTRequest login $ B.all <&> head >>= L.all <&> head >>= C.all
    print dat

