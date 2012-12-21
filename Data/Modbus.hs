module Data.Modbus 
  ( ModbusResponse(..)
  , readHoldingRegisters
  , readCoils
  , modbusQuery
  , pack16
  , pack32
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
import Control.Exception (IOException, handle)
import Data.Maybe


-- | the Modbus response from the server 
data ModbusResponse = ModbusResponse
    { mSlaveId :: Word8
    , mCode    :: Word8
    , mPayload :: [Word8]
    , mCrc     :: [Word8]
    } deriving Show


-- | read holding registers
readHoldingRegisters :: Int -> Int -> Int -> [Word8]
readHoldingRegisters sid addr cnt =
    addCRC [ fromIntegral sid
           , 0x03
           , hiByte addr'
           , loByte addr'
           , hiByte cnt'
           , loByte cnt'
           ]
    where addr' = fromIntegral addr
          cnt'  = fromIntegral cnt


-- | read coils
readCoils :: Int -> Int -> Int -> [Word8]
readCoils sid addr cnt =
    addCRC [ fromIntegral sid
           , 0x01
           , hiByte addr'
           , loByte addr'
           , hiByte cnt'
           , loByte cnt'
           ]
    where addr' = fromIntegral addr
          cnt'  = fromIntegral cnt


-- Modbus CRC is little-endian on the wire
addCRC :: [Word8] -> [Word8]
addCRC msg = msg ++ [loByte crc, hiByte crc] where crc = crc16 msg

hiByte :: Word16 -> Word8
hiByte = fromIntegral . (`shiftR` 8)

loByte :: Word16 -> Word8
loByte = fromIntegral . (.&. 0xff)

upShift :: [Word8] -> Int
upShift = foldl' acc 0
  where acc a o = (a `shiftL` 8) .|. fromIntegral o

-- | pack a list of Word8s into a list of Word16s
pack16 :: [Word8] -> [Word16]
pack16 = map (fromIntegral . upShift) . chunk 2

-- | pack a list of Word8s into a list of Word32s
pack32 :: [Word8] -> [Word32]
pack32 = map (fromIntegral . upShift) . chunk 4

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = y1 : chunk n y2 where (y1,y2) = splitAt n xs

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
modbusQuery :: HostName -> Int -> [Word8]
               -> IO (Either String ModbusResponse)
modbusQuery host port command = 
    handle (\e -> return (Left $ show (e :: IOException))) $ do
        sock <- openSocket host port
        send sock (BL.pack command)
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
    code    <- anyWord8
    payload <- modbusPayload code
    crc     <- takeWord8 2
    return $ ModbusResponse slaveId code payload crc
  where modbusPayload code
          | code `elem` [1, 2, 3, 4] = do
              n   <- anyWord8
              msg <- takeWord8 (fromIntegral n)
              return $ n : msg
          | code `elem` [5, 6, 15, 16]   = takeWord8 4
          | code == 22                   = takeWord8 6
          | code >= 0x80 && code <= 0xff = takeWord8 1
          | otherwise = mzero
        takeWord8 n = B.unpack `fmap` AL.take n
