module Main where

import Data.Modbus
import Network
import Network.Socket
import Control.Exception

main :: IO ()
main = 
  let host = "75.149.170.14"
      port = 502
      timeout = 500000
      serverId = 1
      address = 120
      registerCount = 8
      req = AduRequest serverId $ ReadHoldingRegisters address registerCount
  in
    withSocket host port $ \s -> do
        resp <- modFunction (networkTransport s) timeout req
        print resp

-- | open a socket and run the specified action
withSocket :: HostName -> Int -> (Socket -> IO ()) -> IO ()
withSocket host port = 
    bracket
      (do addrinfos <- getAddrInfo Nothing (Just host) (Just $ show port)
          let serveraddr = head addrinfos
          sock <- socket (addrFamily serveraddr) Stream defaultProtocol
          connect sock (addrAddress serveraddr)
          return sock)
      sClose
