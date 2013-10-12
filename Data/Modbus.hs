module Data.Modbus
  ( ModRequest(..)
  , ModResponse(..)
  , ModRequestFrame(..)
  , ModResponseFrame(..)
  , ExceptionCode(..)
  , mkException
  , matches
  , ModRegister
  , SlaveId
  , FunctionCode
  ) where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Digest.CRC16
import Data.Serialize
import Data.Word

type ModRegister = Word16
type SlaveId = Word8
type FunctionCode = Word8

data ModRequestFrame = ModRequestFrame SlaveId ModRequest deriving (Show)
data ModResponseFrame = ModResponseFrame SlaveId ModResponse deriving (Show)

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

-- | Check that the given response is appropriate for the given request.
matches :: ModRequest -> ModResponse -> Bool
matches req res = case (req, res) of
    (ReadCoils{},                 ReadCoilsResponse{})                 -> True
    (ReadDiscreteInputs{},        ReadDiscreteInputsResponse{})        -> True
    (ReadHoldingRegisters _ a,    ReadHoldingRegistersResponse b _)    -> 
        fromIntegral b == 2 * a -- 2 response bytes per point
    (ReadInputRegisters{},        ReadInputRegistersResponse{})        -> True
    (WriteSingleCoil a _,         WriteSingleCoilResponse b _)         -> a == b 
    (WriteSingleRegister a _,     WriteSingleRegisterResponse b _)     -> a == b
    (WriteDiagnosticRegister a _, WriteDiagnosticRegisterResponse b _) -> a == b
    (WriteMultipleCoils{},        WriteMultipleCoilsResponse{})        -> True
    (WriteMultipleRegisters{},    WriteMultipleRegistersResponse{})    -> True
    -- TODO: should check that request fn code matches exception 
    (_,                           ExceptionResponse{})                 -> True
    (_,                           UnknownFunctionResponse{})           -> True
    _                                                                  -> False

data ModRequest 
    = ReadCoils ModRegister Word16
    | ReadDiscreteInputs ModRegister Word16
    | ReadHoldingRegisters ModRegister Word16
    | ReadInputRegisters ModRegister Word16
    | WriteSingleCoil ModRegister Word16
    | WriteSingleRegister ModRegister Word16
    | WriteDiagnosticRegister Word16 Word16
    | WriteMultipleCoils ModRegister Word16 Word8 ByteString
    | WriteMultipleRegisters ModRegister Word16 Word8 ByteString
    deriving (Show)

instance Serialize ModRequest where
    get = do 
        fn <- getWord8
        case fn of
            1  -> f ReadCoils
            2  -> f ReadDiscreteInputs
            3  -> f ReadHoldingRegisters
            4  -> f ReadInputRegisters
            5  -> f WriteSingleCoil
            6  -> f WriteSingleRegister
            8  -> f WriteDiagnosticRegister
            15 -> f' WriteMultipleCoils
            16 -> f' WriteMultipleRegisters
            _  -> fail $ "Unsupported function code: " ++ show fn
      where
        f cons = cons <$> getWord16be <*> getWord16be
        f' cons = do
            addr  <- getWord16be
            quant <- getWord16be
            count <- getWord8
            body  <- getBytes (fromIntegral count)
            return $ cons addr quant count body
    put req = case req of
        (ReadCoils addr cnt)                -> f 1 addr cnt
        (ReadDiscreteInputs addr cnt)       -> f 2 addr cnt
        (ReadHoldingRegisters addr cnt)     -> f 3 addr cnt
        (ReadInputRegisters addr cnt)       -> f 4 addr cnt
        (WriteSingleCoil addr cnt)          -> f 5 addr cnt
        (WriteSingleRegister addr cnt)      -> f 6 addr cnt
        (WriteDiagnosticRegister subfn dat) -> f 8 subfn dat
        (WriteMultipleCoils addr qnt cnt b)      -> f' 15 addr qnt cnt b
        (WriteMultipleRegisters addr qnt cnt b)  -> f' 16 addr qnt cnt b
      where
        f fn w1 w2 = putWord8 fn >> putWord16be w1 >> putWord16be w2
        f' fn addr qnt cnt b = putWord8 fn >> putWord16be addr >>
            putWord16be qnt >> putWord8 cnt >> putByteString b 

data ModResponse 
    = ReadCoilsResponse Word8 ByteString
    | ReadDiscreteInputsResponse Word8 ByteString
    | ReadHoldingRegistersResponse Word8 ByteString
    | ReadInputRegistersResponse Word8 ByteString
    | WriteSingleCoilResponse ModRegister Word16
    | WriteSingleRegisterResponse ModRegister Word16
    | WriteDiagnosticRegisterResponse Word16 Word16
    | WriteMultipleCoilsResponse ModRegister Word16
    | WriteMultipleRegistersResponse ModRegister Word16
    | ExceptionResponse FunctionCode ExceptionCode
    | UnknownFunctionResponse FunctionCode
    deriving (Show)

instance Serialize ModResponse where
    get = do 
        fn <- getWord8
        case fn of
            1  -> f ReadCoilsResponse
            2  -> f ReadDiscreteInputsResponse
            3  -> f ReadHoldingRegistersResponse
            4  -> f ReadInputRegistersResponse
            5  -> f' WriteSingleCoilResponse
            6  -> f' WriteSingleRegisterResponse
            8  -> f' WriteDiagnosticRegisterResponse
            15 -> f' WriteMultipleCoilsResponse
            16 -> f' WriteMultipleRegistersResponse
            x | x >= 0x80 -> ExceptionResponse (x - 0x80) <$> get
            _  -> return $ UnknownFunctionResponse fn
      where
        f cons = do
            count <- getWord8
            body  <- getBytes (fromIntegral count)
            return $ cons count body
        f' cons = do
            addr <- getWord16be
            body <- getWord16be
            return $ cons addr body
    put req = case req of
        (ReadCoilsResponse cnt b)            -> f 1 cnt b
        (ReadDiscreteInputsResponse cnt b)   -> f 2 cnt b
        (ReadHoldingRegistersResponse cnt b) -> f 3 cnt b
        (ReadInputRegistersResponse cnt b)   -> f 4 cnt b
        (WriteSingleCoilResponse addr b)     -> f' 5 addr b
        (WriteSingleRegisterResponse addr b) -> f' 6 addr b
        (WriteDiagnosticRegisterResponse subfn dat) -> 
            putWord8 8 >> putWord16be subfn >> putWord16be dat
        (WriteMultipleCoilsResponse addr b)     -> f' 15 addr b
        (WriteMultipleRegistersResponse addr b) -> f' 16 addr b
        (ExceptionResponse fn ec)    -> put fn >> put ec
        (UnknownFunctionResponse fn) -> put fn
      where
        f fn cnt b = putWord8 fn >> putWord8 cnt >> putByteString b
        f' fn addr b = putWord8 fn >> putWord16be addr >> putWord16be b


data ExceptionCode 
    = IllegalFunction
    | IllegalDataAddress
    | IllegalDataValue
    | SlaveDeviceFailure
    | Acknowledge
    | SlaveDeviceBusy
    | MemoryParityError
    | GatewayPathUnavailable
    | GatewayTargetFailedToRespond
    | UnknownExceptionCode Word8
    deriving Show

instance Serialize ExceptionCode where
    put ec = putWord8 $ case ec of
        IllegalFunction              -> 0x01
        IllegalDataAddress           -> 0x02
        IllegalDataValue             -> 0x03
        SlaveDeviceFailure           -> 0x04
        Acknowledge                  -> 0x05
        SlaveDeviceBusy              -> 0x06
        MemoryParityError            -> 0x08
        GatewayPathUnavailable       -> 0x0A
        GatewayTargetFailedToRespond -> 0x0B
        (UnknownExceptionCode x)     -> x
    get = do
        c <- getWord8
        return $ case c of
          0x01 -> IllegalFunction
          0x02 -> IllegalDataAddress
          0x03 -> IllegalDataValue
          0x04 -> SlaveDeviceFailure
          0x05 -> Acknowledge
          0x06 -> SlaveDeviceBusy
          0x08 -> MemoryParityError
          0x0A -> GatewayPathUnavailable
          0x0B -> GatewayTargetFailedToRespond
          x    -> UnknownExceptionCode x

mkException :: SlaveId -> ExceptionCode -> ByteString
mkException slaveId t = encode $ 
    ModResponseFrame slaveId $ ExceptionResponse 0x81 t
