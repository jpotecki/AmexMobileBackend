{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}

module Lib where

import Control.Lens             (to, only,(^?),ix, toListOf, (^.), makeLenses)
import Data.ByteString.Lazy     (toStrict, ByteString)
import Data.ByteString          (isInfixOf)
import Data.Text                (Text, unpack, toUpper, stripSuffix, strip, replace)
import Data.Text.Read           (decimal, Reader(..))
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding  (decodeUtf8With)
import Network.HTTP.Client      (Response)
import Network.Wreq             (responseBody, get)
import Text.Taggy               (Node)
import Text.Taggy.Lens          (html, elements, children, contents, allNamed, attributed)
import Data.Maybe               (catMaybes, isJust, fromJust)
import Data.List                (union, (\\))
import Debug.Trace
import GHC.Generics hiding (to)
import Data.Aeson

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


type Stores = [Store]


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


instance ToJSON FPattern
instance FromJSON FPattern


instance Show FPattern where
    show Contain = "contain"
    show BeginWi = "begin_with"


instance Show Req where
    show Req{..} =
        concat [ "http://akzeptanz.amex-services.de/suche.php?"
           , "zip_code="
           , show zip
           , "&zip_hidden="
           , "&within_a_distance_of="
           , show distance
           , "&city="
           , unpack . toUpper $ city
           , "&city_selected="
           , unpack . toUpper $ city
           , "&agreement_code=&industry_code=&industry_code_md5="
           , "&firma="
           , unpack name
           , "&firma_pattern="
           , show firma_pattern
           , "&page="
           , show page
           , "&agreement_code_md5="
           , show business
           ] 


data Business = All
              | CarRent
              | Hotel
              | Food
              | Leasure
              | Travel
              | Gas
              | Other 
              deriving (Eq, Generic)

instance ToJSON Business
instance FromJSON Business


instance Show Business where
    show All     = ""
    show CarRent = "60753bc681d752341358cdbcc49c008b"
    show Hotel   = "9cd6d03105117ff67d7108419cfef5a1"  
    show Food    = "00e276643d45f57c18187dafe623b0c6" 
    show Leasure = "ea4e56623bab524e1e613e29d9a7eadd"
    show Travel  = "d13520d4dc3202c456c3a4047db19a90"
    show Gas     = "7fee34a8555a5ad20603db6ad7a99d69"
    show Other   = "13dab36f0d5c45f0bc86e3eeae3084a6"

        

sampleURL :: Req
sampleURL = Req 12627 "Berlin" 0 "http://akzeptanz.amex-services.de/suche.php" 20 BeginWi All "Mc"


getResult :: [Store] -> Req -> ([Store] -> IO a) -> IO [Store]
getResult res1 req@Req{..} f = do
    content  <- get $ show req
    let res2 =  filterDist distance $ catMaybes . stores' $ content
        diff =  res2 \\ res1
        uni' = res1 `union` res2
    f diff
    if length diff == 0
        then return $ uni'
        else getResult uni' (nextpage req) f
  where 
        nextpage :: Req -> Req
        nextpage x@Req{..} = x { page = succ page }
                                  

filterDist :: Int -> [Store] -> [Store]
filterDist dist = filter (lessDist dist)
  where lessDist :: Int -> Store -> Bool
        lessDist dist Store{..} = sdist <= dist
    

table :: [Node] -> Maybe Store
table row = do  name    <- row ^? ix 0 . elements . contents
                phone   <- row ^? ix 1 . contents
                zip     <- row ^? ix 2 . contents
                city    <- row ^? ix 3 . contents
                address <- row ^? ix 3 . elements . contents
                dist    <- row ^? ix 3 . elements . elements. contents
                let dist' = stripSuffix "km" (strip dist) >>= \x -> readInt' $ strip x
                    zip'  = readInt' $ strip zip
                 in if isJust dist' && isJust zip' 
                    then return $ Store name (replace " " "" phone) (fromJust zip') city address (fromJust dist')
                    else  Nothing


readInt' :: Text -> Maybe Int
readInt' txt =
    let read = decimal txt
     in case read of
         Right (i, _ ) -> Just i
         Left _        -> Nothing


stores' :: Response ByteString -> [Maybe Store]
stores' = toListOf
            $ responseBody . to (decodeUtf8With lenientDecode)
            . html . allNamed (only "tr" ) . attributed (ix "class" . only "item")  . children . to table


getExample :: IO ()
getExample = do res <- getResult [] sampleURL $ f
                mapM_ print res
  where
      f :: [Store] -> IO ()
      f = mapM_ print