{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}

module Amex.Response where 

import Data.Aeson
import GHC.Generics
import Data.Text (Text)


data Store = Store { name   :: Text
                   , phone  :: Text
                   , zip    :: Int
                   , city   :: Text
                   , address:: Text
                   , dist   :: Int
                   } deriving (Eq, Generic, Show)

instance ToJSON Store where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Store