{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Google.Distance (getDistance) where

import qualified Data.Aeson as JSON
import           Data.Aeson.Lens (_String, key, _Double, nth)
import qualified Data.Text  as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import Data.List (intersperse)
import Control.Lens


import Network.Wreq
import Network.HTTP.Client      (Response, responseStatus)

-- import APIKey
import Amex.Response

type Lat = Double
type Lon = Double

show' :: Show a => a -> T.Text
show' = T.pack . show

getReqURL :: (Lat, Lon) -> Store -> String
getReqURL (lat, lon) Store{..} = 
  let url = "https://maps.googleapis.com/maps/api/distancematrix/json"
      origin = T.concat [show' lat, ",", show' lon ]
      destin = T.concat $ intersperse "+" [name, address, show' zip, city]
   in T.unpack . T.concat $ [ url, "?origins=", origin, "&destinations=", destin
                            , "&units=", "metric" ]


getDistance :: (Lat, Lon) -> Store -> IO (Maybe Double)
getDistance origin store = do
  res <- get (getReqURL origin store)
  return $ res ^? responseBody . key "rows" . nth 0 . key "elements" . nth 0 . key "distance" . key "value" . _Double