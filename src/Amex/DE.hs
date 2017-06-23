{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}

module Amex.DE (getResult, getExample) where

import Prelude hiding (init)
import Control.Lens             (to, only,(^?),ix, toListOf, (^.), makeLenses)
import Data.ByteString.Lazy     (toStrict, ByteString)
import Data.ByteString          (isInfixOf)
import Data.Text                (Text, unpack, toUpper, stripSuffix, strip, replace, init)
import Data.Text.Read           (decimal, Reader(..), rational)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding  (decodeUtf8With)
import Network.HTTP.Client      (Response)
import Network.Wreq             (responseBody, get)
import Text.Taggy               (Node)
import Text.Taggy.Lens          (html, elements, children, contents, allNamed, attributed)
import Data.Maybe               (catMaybes, isJust, fromJust, isNothing)
import Data.List                (union, (\\))
import Debug.Trace
import GHC.Generics hiding (to)
import Data.Aeson
import Amex.Response (Store(..))
import Amex.Request
import Google.Distance
import Control.Concurrent (forkIO)
import Network.CGI
import Control.Monad (forM)
import           Control.Concurrent.Async



type Lat = Double
type Lon = Double

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
sampleURL = Req 12627 "Berlin" 0 "http://akzeptanz.amex-services.de/suche.php" 20 BeginWi All "Mc" Nothing Nothing

-- TODO: Do another request to the GoogleMaps API and get
--       the actual distance to the user by using lat/lon

getResult :: Req                 -- request which should be executed
          -> ([Store] -> IO a)   -- applied to each partial result
          -> IO [Store]          -- resulting list of all stores
-- ^ recursivly polling the store results, the function f is called on all `new` results
getResult req f = getResult' [] req f

getResult' :: [Store] -> Req -> ([Store] -> IO a) -> IO [Store]
getResult' acc req@Req{..} f = do
    content  <- get $ show req
    let res2 =  filterDist distance $ catMaybes . stores' $ content
        uni' = acc `union` res2
    -- fork a thread and handle the new data 
    done <- async$ forkIO $ morePrecision (lat, lon) f $ uni' \\ acc
    _ <- wait done
    -- f $ uni' \\ acc
    if length uni' == length acc
        then return uni'
        else getResult' uni' (nextpage req) f
  where 
        nextpage :: Req -> Req
        nextpage x@Req{..} = x { page = succ page }


morePrecision :: (Maybe Lat, Maybe Lon) -> ([Store] -> IO a) -> [Store] -> IO ()
morePrecision (lat, lon) f stores = 
  if (isNothing lat || isNothing lon)
    then f stores >> return ()
    else forM stores updateStores >>= f >> return ()
  where
    updateStores :: Store -> IO Store
    updateStores store = do
      dist <- getDistance (fromJust lat, fromJust lon) store
      case dist of 
        Nothing -> return store 
        Just  x -> return $ store { dist = x / 1000 }




filterDist :: Double -> [Store] -> [Store]
filterDist dist = filter (lessDist dist)
  where lessDist :: Double -> Store -> Bool
        lessDist maxdist Store{..} = dist <= maxdist



table :: [Node] -> Maybe Store
table row = do  name    <- row ^? ix 0 . elements . contents 
                phone   <- row ^? ix 1 . contents
                zip     <- row ^? ix 2 . contents
                city    <- row ^? ix 3 . contents
                address <- row ^? ix 3 . elements . contents
                dist    <- row ^? ix 3 . elements . elements. contents
                let dist' = stripSuffix "km" (strip dist) >>= readDouble' . strip
                    zip'  = readInt' $ strip zip
                 in if isJust dist' && isJust zip' 
                    then return $ Store name (replace " " "" phone) (fromJust zip') (init city) address (fromJust dist')
                    else  Nothing


readDouble' :: Text -> Maybe Double
readDouble' str =
  case rational str of
    Right (d, _) -> Just d
    Left _       -> Nothing

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
-- ^ example function which will poll some results and print it
getExample = do res <- getResult sampleURL $ f
                mapM_ print res
  where
      f :: [Store] -> IO ()
      f = mapM_ print