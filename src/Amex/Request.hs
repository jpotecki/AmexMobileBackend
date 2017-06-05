{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}

module Amex.Request where


import Data.Text
import Data.Aeson
import GHC.Generics

data Req = Req  { zip           :: Int
                , city          :: Text
                , page          :: Int
                , url           :: Text
                , distance      :: Int
                , firma_pattern :: FPattern
                , business      :: Business
                , name          :: Text
                } deriving (Eq, Generic)

instance ToJSON Req where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Req

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