module Main where

import Data.Modbus
import Network
import Network.Socket

main :: IO ()
main = do
    sock <- openSocket "hostname" 502
    resp <- modFunction (networkTransport sock) timeout req
    print resp
    sClose sock
  where
    timeout = 50000
    req = AduRequest 1 $ ReadHoldingRegisters 120 8

-- | open a socket to the given host and port
openSocket :: HostName -> Int -> IO Socket
openSocket host port = do
  addrinfos <- getAddrInfo Nothing (Just host) (Just $ show port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  return sock
