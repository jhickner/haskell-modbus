module Data.Modbus
  ( ModRequest(..)
  , ModResponse(..)
  , ExceptionCode(..)
  , matches
  , ModRegister
  , FunctionCode
  ) where

import           Control.Monad
import           Data.Array.BitArray
import           Data.Array.BitArray.ByteString
import qualified Data.ByteString as BS
import           Data.Serialize
import           Data.Word

type ModRegister = Word16
type FunctionCode = Word8

-- | Check that the given response is appropriate for the given request.
matches :: ModRequest -> ModResponse -> Bool
matches req res = case (req, res) of
    (ReadCoils{},                 ReadCoilsResponse{})                 -> True
    (ReadDiscreteInputs{},        ReadDiscreteInputsResponse{})        -> True
    (ReadHoldingRegisters _ a,    ReadHoldingRegistersResponse b)      -> fromIntegral a == length b
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
    = ReadCoils {readCoilsModReg :: ModRegister, readCoilsCnt :: Word16}
    | ReadDiscreteInputs {readDiscreteInputsModReg :: ModRegister, readDiscreteInputsCnt :: Word16}
    | ReadHoldingRegisters {readHoldingRegistersModReg :: ModRegister, readHoldingRegistersCnt :: Word16}
    | ReadInputRegisters {readInputRegistersModReg :: ModRegister, readInputRegistersCnt :: Word16}
    | WriteSingleCoil {writeSingleCoilModReg :: ModRegister, writeSingleCoilCnt :: Word16}
    | WriteSingleRegister {writeSingleRegisterModReg :: ModRegister, writeSingleRegister :: Word16}
    | WriteDiagnosticRegister {writeDiagnosticRegisterSubFcn :: Word16, writeDiagnosticRegisterDat :: Word16}
    | WriteMultipleCoils {writeMultipleCoilsModReg :: ModRegister, qWriteMultipleCoilsVal :: [Bool]}
    | WriteMultipleRegisters {writeMultipleRegistersModReg :: ModRegister, writeMultipleRegistersVal :: [Word16]}
    deriving (Eq, Show)

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
            15 -> f'
            16 -> f''
            _  -> fail $ "Unsupported function code: " ++ show fn
      where
        f cons = cons <$> getWord16be <*> getWord16be

        f' = do
          addr  <- getWord16be
          quant <- getWord16be
          count <- getWord8
          body  <- getBytes (fromIntegral count)
          let a = fromByteString (0 :: Int, fromIntegral quant - 1) body
          return $ WriteMultipleCoils addr (elems a)

        f'' = do
            addr  <- getWord16be
            quant <- getWord16be
            _ <- getWord8
            vals  <- replicateM (fromIntegral quant) getWord16be
            return $ WriteMultipleRegisters addr vals

    put req = case req of
        (ReadCoils addr cnt)                -> f 1 addr cnt
        (ReadDiscreteInputs addr cnt)       -> f 2 addr cnt
        (ReadHoldingRegisters addr cnt)     -> f 3 addr cnt
        (ReadInputRegisters addr cnt)       -> f 4 addr cnt
        (WriteSingleCoil addr cnt)          -> f 5 addr cnt
        (WriteSingleRegister addr cnt)      -> f 6 addr cnt
        (WriteDiagnosticRegister subfn dat) -> f 8 subfn dat
        (WriteMultipleCoils addr vals)      -> f' 15 addr vals
        (WriteMultipleRegisters addr vals)  -> f'' 16 addr vals
      where
        f fn w1 w2 = putWord8 fn >> putWord16be w1 >> putWord16be w2

        f' fn addr vals = do
          putWord8 fn
          putWord16be addr
          putWord16be (fromIntegral $ length vals)
          putWord8 (fromIntegral $ (7 + length vals) `div` 8)
          let a = listArray (0, length vals - 1) vals
          putByteString $ toByteString a

        f'' fn addr vals = do
          putWord8 fn
          putWord16be addr
          putWord16be (fromIntegral $ length vals)
          putWord8 (fromIntegral $ 2 * length vals)
          mapM_ putWord16be vals



data ModResponse
    = ReadCoilsResponse {readCoilsResponseVal :: [Bool]}
    | ReadDiscreteInputsResponse {readDiscreteInputsResponseVal :: [Bool]}
    | ReadHoldingRegistersResponse {readHoldingRegistersResponseVal :: [Word16]}
    | ReadInputRegistersResponse {readInputRegistersVal :: [Word16]}
    | WriteSingleCoilResponse {writeSingleCoilResponseModReg :: ModRegister, writeSingleCoilResponseVal :: Word16}
    | WriteSingleRegisterResponse {writeSingleRegisterResponseModReg :: ModRegister, writeSingleRegisterResponseVal :: Word16}
    | WriteDiagnosticRegisterResponse {writeDiagnosticRegisterResponseSubFcn :: Word16, writeDiagnosticRegisterResponseDat :: Word16}
    | WriteMultipleCoilsResponse {writeMultipleCoilsResponseModReg :: ModRegister, writeMultipleCoilsResponseVal :: Word16}
    | WriteMultipleRegistersResponse {writeMultipleRegistersResponseModReg :: ModRegister, writeMultipleRegistersResponseVal :: Word16}
    | ExceptionResponse FunctionCode ExceptionCode
    | UnknownFunctionResponse FunctionCode
    deriving (Eq, Show)

instance Serialize ModResponse where
    get = do
        fn <- getWord8
        case fn of
            1  -> f ReadCoilsResponse
            2  -> f ReadDiscreteInputsResponse
            3  -> f' ReadHoldingRegistersResponse
            4  -> f' ReadInputRegistersResponse
            5  -> f'' WriteSingleCoilResponse
            6  -> f'' WriteSingleRegisterResponse
            8  -> f'' WriteDiagnosticRegisterResponse
            15 -> f'' WriteMultipleCoilsResponse
            16 -> f'' WriteMultipleRegistersResponse
            x | x >= 0x80 -> ExceptionResponse (x - 0x80) <$> get
            _  -> return $ UnknownFunctionResponse fn
      where
        f cons = do
          count <- getWord8
          body  <- getBytes (fromIntegral count)
          let a = fromByteString (0 :: Int, 8 * BS.length body - 1) body
          return $ cons (elems a)
        f' cons = do
          count <- getWord8
          ws <- replicateM (fromIntegral $ count `div` 2) getWord16be
          return $ cons ws
        f'' cons = do
            addr <- getWord16be
            body <- getWord16be
            return $ cons addr body

    put req = case req of
        (ReadCoilsResponse vals) -> f 1 vals
        (ReadDiscreteInputsResponse vals) -> f 2 vals
        (ReadHoldingRegistersResponse vals) -> f' 3 vals
        (ReadInputRegistersResponse vals)   -> f' 4 vals
        (WriteSingleCoilResponse addr b)     -> f'' 5 addr b
        (WriteSingleRegisterResponse addr b) -> f'' 6 addr b
        (WriteDiagnosticRegisterResponse subfn dat) ->
            putWord8 8 >> putWord16be subfn >> putWord16be dat
        (WriteMultipleCoilsResponse addr b)     -> f'' 15 addr b
        (WriteMultipleRegistersResponse addr b) -> f'' 16 addr b
        (ExceptionResponse fn ec)
                       |fn >= 0x80    -> put fn >> put ec
                       |otherwise     -> put (fn + 0x80) >> put ec
        (UnknownFunctionResponse fn) -> put fn
      where
        f fn vals = do
          putWord8 fn
          putWord8 (fromIntegral $ (7 + length vals) `div` 8)
          let a = listArray (0, length vals - 1) vals
          putByteString $ toByteString a
        f' fn vals = do
          putWord8 fn
          putWord8 (fromIntegral $ 2 * length vals)
          mapM_ putWord16be vals
        f'' fn addr b = putWord8 fn >> putWord16be addr >> putWord16be b


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
    | UnknownExceptionCode {getUnknownException :: Word8}
    deriving (Eq, Show)

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
