module Data.Modbus 
  ( ModbusResponse
  , SlaveId
  , Address
  , Count
  , ModbusCommand
  , readHoldingRegisters
  , readCoils
  , modbusQuery
  , octetsToWord32
  ) where


import System.Timeout

import Network
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy

import Data.Digest.CRC16
import Data.Attoparsec.Lazy as AL
import Data.Word
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.List (foldl')

import Control.Monad
import Data.Maybe


-- | a Modbus slave id
type SlaveId = Word8

-- | a register or coil address 
type Address = Word16

-- | the number of registers or coils to read
type Count   = Word16

-- | a bytestring representing a prepared Modbus command
type ModbusCommand = BL.ByteString

-- | the Modbus response from the server 
data ModbusResponse = ModbusResponse
    { mSlaveId :: Word8
    , mCode    :: Word8
    , mPayload :: [Word8]
    , mCrc     :: [Word8]
    } deriving Show


-- | read holding registers
readHoldingRegisters :: SlaveId -> Address -> Count -> ModbusCommand
readHoldingRegisters sid addr cnt =
    BL.pack $ addCRC [ sid
                     , 0x03
                     , hiByte addr
                     , loByte addr
                     , hiByte cnt
                     , loByte cnt
                     ]


-- | read coils
readCoils :: SlaveId -> Address -> Count -> ModbusCommand
readCoils sid addr cnt =
    BL.pack $ addCRC [ sid
                     , 0x01
                     , hiByte addr
                     , loByte addr
                     , hiByte cnt
                     , loByte cnt
                     ]


-- Modbus CRC is little-endian on the wire
addCRC :: [Word8] -> [Word8]
addCRC msg = msg ++ [loByte crc, hiByte crc] where crc = crc16 msg

hiByte :: Word16 -> Word8
hiByte = fromIntegral . (`shiftR` 8)

loByte :: Word16 -> Word8
loByte = fromIntegral . (.&. 0xff)

-- | convert a list of (4) Word8s into a single Word32
octetsToWord32  :: [Word8] -> Word32
octetsToWord32 = foldl' acc 0
  where acc a o = (a `shiftL` 8) .|. fromIntegral o


-- | open a socket to the given host and port
openSocket :: HostName -> Int -> IO Socket
openSocket host port = do
  addrinfos <- getAddrInfo Nothing (Just host) (Just $ show port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  timeout 500000 $ connect sock (addrAddress serveraddr)
  return sock


-- | read a lazy bytestring from a socket with a timeout
readSock :: Socket -> IO BL.ByteString
readSock sock = do
  res <- timeout 500000 $ recv sock 512
  return $ fromMaybe BL.empty res


-- | open a socket, send a modbus query and retrieve the response
modbusQuery :: HostName -> Int -> ModbusCommand 
               -> IO (Either String ModbusResponse)
modbusQuery host port command = do
    sock <- openSocket host port
    send sock command
    res <- readSock sock
    let parsed = parseBS res >>= checkCRC >>= checkCode
    sClose sock
    return parsed
  where parseBS bs = eitherResult $ parse modbusResponse bs
  

-- | check ModbusResponse crc
checkCRC :: ModbusResponse -> Either String ModbusResponse
checkCRC p = if mCrc p == [loByte crc, hiByte crc]
             then Right p
             else Left "crc check failed"
           where crc = crc16 $ mSlaveId p : mCode p : mPayload p


-- | check for error codes
checkCode :: ModbusResponse -> Either String ModbusResponse
checkCode p = if mCode p >= 0x80
              then Left "modbus server returned an error"
              else Right p
              

-- | attoparsec parser for modbus response packets
modbusResponse :: Parser ModbusResponse 
modbusResponse = do
    slaveId <- anyWord8
    code <- anyWord8
    payload <- modbusPayload code
    crc <- takeWord8 2
    return $ ModbusResponse slaveId code payload crc
  where modbusPayload code
          | code `elem` [1, 2, 3, 4] = do
              n <- anyWord8
              msg <- takeWord8 (fromIntegral n)
              return $ n : msg
          | code `elem` [5, 6, 15, 16]   = takeWord8 4
          | code == 22                   = takeWord8 6
          | code >= 0x80 && code <= 0xff = takeWord8 1
          | otherwise = mzero
        takeWord8 n = B.unpack `fmap` AL.take n
