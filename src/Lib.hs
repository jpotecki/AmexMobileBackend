{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lib (someFunc) where

import Control.Lens             (to, only,(^?),ix, toListOf, (^.), makeLenses)
import Data.ByteString.Lazy     (toStrict, ByteString)
import Data.ByteString          (isInfixOf)
import Data.Text                (Text, unpack, toUpper, stripSuffix)
import Data.Text.Read           (decimal)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding  (decodeUtf8With)
import Network.HTTP.Client      (Response)
import Network.Wreq             (responseBody, get)
import Text.Taggy               (Node)
import Text.Taggy.Lens          (html, elements, children, contents, allNamed, attributed)
import Data.Maybe               (catMaybes)
import Data.List                (union)
import Data.Either.Combinators  (rightToMaybe)

data Store = Store { sname   :: Text 
                   , sphone  :: Int 
                   , szip    :: Text 
                   , saddress:: Text 
                   , sdist   :: Int 
                   } deriving (Show, Eq)

type Stores = [Store]

data Req = Req  { zip           :: Int
                , city          :: Text
                , page          :: Int
                , url           :: Text
                , distance      :: Int
                , firma_pattern :: FPattern
                , business      :: Business
                , name          :: Text
                } 


data FPattern = Contain 
              | BeginWi 


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




instance Show Business where
    show All     = ""
    show CarRent = "60753bc681d752341358cdbcc49c008b"
    show Hotel   = "9cd6d03105117ff67d7108419cfef5a1"  
    show Food    = "00e276643d45f57c18187dafe623b0c6" 
    show Leasure = "ea4e56623bab524e1e613e29d9a7eadd"     
        
        
-- "http://akzeptanz.amex-services.de/suche.php?zip_code=12000&zip_hidden=&&within_a_distance_of=0&cityBerlin&city_selected=Berlin&agreement_code=&agreement_code_md5=&industry_code=&industry_code_md5=&firma=Mc&firma_pattern=&page=0"

sampleURL :: Req
sampleURL = Req 12627 "Berlin" 0 "http://akzeptanz.amex-services.de/suche.php" 10 BeginWi Leasure "Mc"


getResults :: Req -> IO [Store]
getResults url = do 
    print $ show url
    content1 <- get $ show url
    content2 <- get $ show . nextpage $ url                   
    let res1    = catMaybes . stores' $ content1
        res2    = catMaybes . stores' $ content2
        res'    = res1 `union` res2
    print res'
    if length res1 == length res'
        then return res1
        else do res3 <- getResults $ (nextpage . nextpage) url
                return $ res' `union` res3

  where nextpage :: Req -> Req
        nextpage x@Req{..} = x { page = succ page }


filterDist :: Int -> [Store] -> [Store]
filterDist dist = filter (lessDist dist)
  where lessDist :: Int -> Store -> Bool
        lessDist dist Store{}
    


table :: [Node] -> Maybe Store
table row = do  name    <- row ^? ix 0 . elements . contents
                phone   <- row ^? ix 1 . contents
                zip     <- row ^? ix 2 . contents
                address <- row ^? ix 3 . contents
                dist    <- row ^? ix 4 . contents
                let dist' = stripSuffix "km" dist
                    zip ' = rightToMaybe $ decimal zip :: Int
                 in if 
                    return $ Store name phone zip address 



stores' :: Response ByteString -> [Maybe Store]
stores' = toListOf
            $ responseBody . to (decodeUtf8With lenientDecode)
            . html . allNamed (only "tr" ) . attributed (ix "class" . only "item")  . children . to table

moreResults :: Response ByteString -> Bool
moreResults response =
    let body = response ^. responseBody
     in "weiteren Ergebnissen" `isInfixOf` (toStrict body)


main :: IO ()
-- main = do content <- get $ show sampleURL
--           mapM_ print (catMaybes . stores'  $ content)
main = do res <- getResults sampleURL
          mapM_ (print . show) res



someFunc = main