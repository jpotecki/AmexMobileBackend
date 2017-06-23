{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}

module Amex.Request where

import Prelude hiding (zip)
import Data.Text hiding (zip)
import Data.Aeson 
import Data.Aeson.Types (typeMismatch)

import GHC.Generics


data Req = Req  { zip           :: Int
                , city          :: Text
                , page          :: Int
                , url           :: Text
                , distance      :: Double
                , firma_pattern :: FPattern
                , business      :: Business
                , name          :: Text
                , lat           :: Maybe Double
                , lon           :: Maybe Double
                } deriving (Eq, Generic)


instance ToJSON Req where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Req where
  parseJSON = withObject "Req" $ \v -> do 
    zip       <-   v.: "zip"
    city      <- v .: "city"
    let page = 0
        url  = "http://akzeptanz.amex-services.de/suche.php"
    distance      <- v .: "distance"
    firma_pattern <- v .: "firma_pattern"
    business      <- v .: "business"
    name          <- v .: "name"
    lat           <- v .:? "lat"
    lon           <- v .:? "lon"
    return Req{..}


data FPattern = Contain 
              | BeginWi 
              deriving (Eq, Generic)


instance ToJSON FPattern where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON FPattern


data Business = All
              | CarRent
              | Hotel
              | Food
              | Leasure
              | Travel
              | Gas
              | Other 
              deriving (Eq, Generic)


instance ToJSON Business where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Business