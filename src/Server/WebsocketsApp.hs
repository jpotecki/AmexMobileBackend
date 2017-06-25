{-# LANGUAGE OverloadedStrings #-}

module Server.WebsocketsApp where

import qualified  Network.WebSockets      as WS
import qualified  Network.WebSockets.Connection      as WS
import qualified  Data.ByteString.Lazy    as LBS
import qualified  Data.ByteString         as BS  hiding (putStrLn)
import qualified  Data.ByteString.Char8   as BS
import qualified  Data.Text               as T          (pack, Text)
import qualified  Data.Aeson              as JSON
import qualified  Amex.Request            as Amex
import qualified  Amex.Response           as Amex
import qualified  Amex.DE                 as Amex
import            Network.Wai
import            Network.HTTP.Types.Status
import            Control.Concurrent.STM
import            Control.Concurrent
import            Control.Concurrent.Async
import            Control.Exception.Safe





data ChanMessage = Message [Amex.Store]
                 | Close
type StoreChan = TChan ChanMessage


ws :: WS.ServerApp
ws pending = do 
  conn <- WS.acceptRequest pending
  putStrLn "Connection via Websockets accepted"
  WS.forkPingThread conn 1
  raw         <- WS.receiveData conn
  BS.putStrLn raw
  case (JSON.eitherDecodeStrict raw :: Either String Amex.Req) of
    Left  err -> invalidRequest  conn err
    Right req -> handleValidConn conn req


invalidRequest :: WS.Connection -> String -> IO ()
invalidRequest conn err = do 
  WS.sendClose conn (statusMessage badRequest400)
  print err


handleValidConn :: WS.Connection -> Amex.Req -> IO ()
handleValidConn conn req = do
  publisher <- atomically $ newTChan
  forkIO $ nightsWatch publisher conn
  Amex.getResult req (sendMSG' publisher) (\_ -> atomically $ writeTChan publisher Close)
  _ <- WS.receiveDataMessage conn
  atomically $ writeTChan publisher Close
  return ()
 where
  sendMSG' :: StoreChan -> [Amex.Store] -> IO ()
  sendMSG' chan = \s -> atomically $ writeTChan chan $ Message s


nightsWatch :: StoreChan -> WS.Connection -> IO ()
nightsWatch chan conn = do
  putStrLn ": Night gathers, and now my watch begins.."
  done <- async $ watch chan conn
  _ <- wait done
  putStrLn  "...And now his watch is ended."
 where
  watch :: StoreChan -> WS.Connection -> IO ()
  watch chan conn = do
    msg <- atomically $ readTChan chan
    case msg of
      Message stores -> do let xs = map (WS.Text . JSON.encode) stores
                            in WS.sendDataMessages conn xs
                           mapM_ print stores
                           watch chan conn
      Close         -> WS.sendClose conn ("Done" :: T.Text) >> return ()
