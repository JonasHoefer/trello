module Trello.Util where

import           Data.Aeson
import           Data.Char
import qualified Data.Map                      as M
import qualified Data.Map.Internal             as M
import           Data.Function
import           Data.List
import           Data.Text                      ( unpack )
import           Data.Functor
import qualified Data.HashMap.Strict           as HM


toQuery :: Value -> M.Map String String
toQuery (Object o) =
    M.unions
        $   (\(k, v) -> M.mapKeysMonotonic (unpack k ++) (toQuery v))
        <$> HM.toList o
toQuery v = M.singleton "" $ case v of
    String t -> unpack t
    Number n -> show n
    Bool   b -> toLower <$> show b
