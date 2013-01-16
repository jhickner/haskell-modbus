module Main where

import Data.Modbus
import qualified System.Hardware.Serialport as SP

main :: IO ()
main = 
  let port = "/dev/ttyUSB0" 
      ss = SP.SerialPortSettings 
        { SP.commSpeed = SP.CS9600
        , SP.bitsPerWord = 8
        , SP.stopb = SP.One
        , SP.parity = SP.Even
        , SP.flowControl = SP.NoFlowControl
        , SP.timeout = 1
        } 
      timeout = 100000
      retries = 3
      serverId = 3
      address = 0x1000
      registerCount = 2
      req = AduRequest serverId $ ReadHoldingRegisters address registerCount
  in do
    s <- SP.openSerial port ss
    resp <- retryModFunction (serialTransport s) timeout retries req
    print resp
    SP.closeSerial s
