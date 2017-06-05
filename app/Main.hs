module Main where

-- import qualified Serverless.AWSLambda as AWS (main)
import qualified Server.Spock as Spock (startServer)

main :: IO ()
main = Spock.startServer 8080