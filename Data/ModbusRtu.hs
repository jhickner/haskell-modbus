module Data.ModbusRtu
  ( ModRequestFrame(..)
  , ModResponseFrame(..)
  , mkException
  , SlaveId
  ) where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Digest.CRC16
import Data.Serialize
import Data.Word
import Data.Modbus

type SlaveId = Word8

data ModRequestFrame = ModRequestFrame { qSlaveId :: SlaveId, qModRequest :: ModRequest} deriving (Show)
data ModResponseFrame = ModResponseFrame { rSlaveId :: SlaveId, rModResponse :: ModResponse} deriving (Show)

instance Serialize ModRequestFrame where
    get = getFrame ModRequestFrame
    put (ModRequestFrame fid req) = putFrame fid req

instance Serialize ModResponseFrame where
    get = getFrame ModResponseFrame
    put (ModResponseFrame fid req) = putFrame fid req

putFrame :: Serialize a => Word8 -> a -> PutM ()
putFrame fid req =
    putWord8 fid >> putByteString body >> putWord16le (crc16 packet)
  where
    body = encode req
    packet = B.unpack $ B.cons fid body

getFrame :: Serialize a => (Word8 -> a -> b) -> Get b
getFrame cons = do
    fid <- get
    req <- get
    crc <- getWord16le
    when (crc /= crc' fid req) $ fail "CRC check failed"
    return $ cons fid req
  where
    crc' fid req = crc16 . B.unpack . B.cons fid $ encode req

mkException :: SlaveId -> ExceptionCode -> ByteString
mkException slaveId t = encode $
    ModResponseFrame slaveId $ ReadCoilsException t
