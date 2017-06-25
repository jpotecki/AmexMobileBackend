{-# LANGUAGE OverloadedStrings #-}


module Server.Spock
    ( startServer
    ) where


import Control.Monad.IO.Class         (liftIO)
import Web.Spock
import Web.Spock.Config
import Network.Wai.Middleware.Cors
import Network.Wai
import Network.WebSockets             (ServerApp, defaultConnectionOptions)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Server.WebsocketsApp           (ws)

data MySession = EmptySession


type Port = Int
type AppM ctx a = SpockCtxM ctx () () () a
type HandlerM ctx a = SpockActionCtx ctx () () () a


startServer :: Port -> IO ()
startServer port = do
  spockCfg <- defaultSpockCfg () (PCNoDatabase) ()
  runSpock port (spock spockCfg $ app ws)


app :: ServerApp -> AppM () ()
app ws = do 
  middleware simpleCors 
  middleware (websocketsOr defaultConnectionOptions ws)
  get root mainSite


mainSite :: HandlerM () ()
mainSite = do
  text "do \nBS.putStrLn Hello World\n\\powered <- by haskell"