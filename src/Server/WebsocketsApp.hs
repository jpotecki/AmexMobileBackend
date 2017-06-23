module Server.WebsocketsApp where

import qualified  Network.WebSockets      as WS
import qualified  Data.ByteString.Lazy    as LBS
import qualified  Data.ByteString         as BS  hiding (putStrLn)
import qualified  Data.ByteString.Char8   as BS
import qualified  Data.Text               as T          (pack)
import qualified  Data.Aeson              as JSON
import qualified  Amex.Request            as Amex
import qualified  Amex.Response           as Amex
import qualified  Amex.DE                 as Amex
import            Network.Wai
import            Network.HTTP.Types.Status



ws :: WS.ServerApp
ws pending = do 
  conn <- WS.acceptRequest pending
  putStrLn "Connection via Websockets accepted"
  WS.forkPingThread conn 50
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
  Amex.getResult req (sendMSG' conn)
  return ()
  -- WS.sendTextData conn status200'
  -- WS.sendClose conn status200'
  -- print "DONE"
 where
  status200' = statusMessage status200
  sendMSG' :: WS.Connection -> [Amex.Store] -> IO ()
  sendMSG' conn = mapM_ (\s -> WS.sendTextData conn $ JSON.encode s)