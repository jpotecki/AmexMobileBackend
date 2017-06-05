{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}

module Amex.Response where 

import Data.Aeson
import GHC.Generics
import Data.Text (Text)


data Store = Store { sname   :: Text 
                   , sphone  :: Text 
                   , szip    :: Int 
                   , scity   :: Text
                   , saddress:: Text 
                   , sdist   :: Int 
                   } deriving (Eq, Generic, Show)

instance ToJSON Store where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Store