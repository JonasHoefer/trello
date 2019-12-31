module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Functor

import           Control.Monad.IO.Class

import           Trello
import           Trello.Util
import qualified Trello.Board                  as B
import qualified Trello.List                   as L
import qualified Trello.Card                   as C
import qualified Trello.Attachment             as A


main :: IO ()
main = do
    login <- fromJust . decode <$> BS.readFile "login.json"
    runTRequest login $ do
        board <- B.all <&> head >>= L.all <&> head
        newCard <- C.post board
        A.post newCard "https://github.com/JonasHoefer/trello"
        img <- A.post newCard "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1c/Haskell-Logo.svg/602px-Haskell-Logo.svg.png"
        C.put $ newCard { C.name = "Card from Haskell",  C.desc = "This Card was created using the Haskell bindings for the `Trello.com` Rest API", C.idAttachmentCover = Just . A.id $ img }
        C.label newCard "purple" (Just "Haskell")
        return ()
