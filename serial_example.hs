module Main where

import Data.Modbus
import qualified System.Hardware.Serialport as SP
import Control.Concurrent

main :: IO ()
main = 
  let port = "/dev/ttyUSB0" 
      ss = SP.SerialPortSettings {SP.commSpeed = SP.CS9600,
            SP.bitsPerWord = 8,
            SP.stopb = SP.One,
            SP.parity = SP.Even,
            SP.flowControl = SP.NoFlowControl,
            SP.timeout = 1} in
  do
    s <- SP.openSerial port ss
    loop s
    SP.closeSerial s
  where
    loop s = 
      let serverId = 3
          address  = 0x1000
          registerCount = 2
          timeoutUs = 100000
          retries = 3 
          modbusRequest = AduRequest serverId $ ReadHoldingRegisters address registerCount in do
        resp <- retryModFunction (serialTransport s) timeoutUs retries modbusRequest
        print $ show resp
        threadDelay 750000
        loop s
