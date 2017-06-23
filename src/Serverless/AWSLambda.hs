{-# LANGUAGE ScopedTypeVariables #-}

module Serverless.AWSLambda where

import qualified Amex.DE                as Amex
import qualified Amex.Request           as Amex
import qualified Amex.Response          as Amex
import Data.Aeson                       as JSON
import qualified Data.ByteString.Char8  as BS
-- import qualified Data.ByteString
import System.IO
import System.Exit

main :: IO ()
main = do
  raw <- BS.hGetLine stdin
  case JSON.eitherDecodeStrict raw of
    Left e                 -> do
      putStrLn "Error reading Lambda input."
      putStrLn $ "Input was " ++ (BS.unpack raw)
      putStrLn $ "Error was " ++ e
      exitFailure
    Right (r :: Amex.Req) -> do 
        Amex.getResult r (mapM_ print) (\xs -> print $ length xs)