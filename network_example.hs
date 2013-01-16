module Main where

import Modbus
import Network
import Network.Socket

main :: IO ()
main = do
    sock <- openSocket "75.149.170.14" 502
    resp <- retryModFunction (networkTransport sock) 500000 3 req
    case resp of
      ModSuccess (AduResponse _ (ReadHoldingRegistersResponse _ x)) -> 
        print x
    sClose sock
  where
    req = AduRequest 1 $ ReadHoldingRegisters 120 8

-- | open a socket to the given host and port
openSocket :: HostName -> Int -> IO Socket
openSocket host port = do
  addrinfos <- getAddrInfo Nothing (Just host) (Just $ show port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  return sock
