module Data.Modbus
  ( ModRequest(..)
  , ModResponse(..)
  , ExceptionCode(..)
  , matches
  , ModRegister
  ) where

import           Control.Monad
import           Data.Array.BitArray
import           Data.Array.BitArray.ByteString
import qualified Data.ByteString as BS
import           Data.Serialize
import           Data.Word

type ModRegister = Word16

-- | Check that the given response is appropriate for the given request.
matches :: ModRequest -> ModResponse -> Bool
matches req res = case (req, res) of
    (ReadCoils{}, ReadCoilsResponse{}) -> True
    (ReadCoils{}, ReadCoilsException{}) -> True
    (ReadDiscreteInputs{}, ReadDiscreteInputsResponse{}) -> True
    (ReadDiscreteInputs{}, ReadDiscreteInputsException{}) -> True
    (ReadHoldingRegisters _ a, ReadHoldingRegistersResponse b) -> fromIntegral a == length b
    (ReadHoldingRegisters{}, ReadHoldingRegistersException{}) -> True
    (ReadInputRegisters{}, ReadInputRegistersResponse{}) -> True
    (ReadInputRegisters{}, ReadInputRegistersException{}) -> True
    (WriteSingleCoil a _, WriteSingleCoilResponse b _) -> a == b
    (WriteSingleCoil{}, WriteSingleCoilException{}) -> True
    (WriteSingleRegister a _, WriteSingleRegisterResponse b _) -> a == b
    (WriteSingleRegister{}, WriteSingleRegisterException{}) -> True
    (WriteDiagnosticRegister a _, WriteDiagnosticRegisterResponse b _) -> a == b
    (WriteDiagnosticRegister{}, WriteDiagnosticRegisterException{}) -> True
    (WriteMultipleCoils{}, WriteMultipleCoilsResponse{}) -> True
    (WriteMultipleCoils{}, WriteMultipleCoilsException{}) -> True
    (WriteMultipleRegisters{}, WriteMultipleRegistersResponse{}) -> True
    (WriteMultipleRegisters{}, WriteMultipleRegistersException{}) -> True
    _ -> False

data ModRequest
    = ReadCoils {readCoilsModReg :: ModRegister, readCoilsCnt :: Word16}
    | ReadDiscreteInputs {readDiscreteInputsModReg :: ModRegister, readDiscreteInputsCnt :: Word16}
    | ReadHoldingRegisters {readHoldingRegistersModReg :: ModRegister, readHoldingRegistersCnt :: Word16}
    | ReadInputRegisters {readInputRegistersModReg :: ModRegister, readInputRegistersCnt :: Word16}
    | WriteSingleCoil {writeSingleCoilModReg :: ModRegister, writeSingleCoilCnt :: Word16}
    | WriteSingleRegister {writeSingleRegisterModReg :: ModRegister, writeSingleRegisterDat :: Word16}
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
    | ReadCoilsException ExceptionCode
    | ReadDiscreteInputsException ExceptionCode
    | ReadHoldingRegistersException ExceptionCode
    | ReadInputRegistersException ExceptionCode
    | WriteSingleCoilException ExceptionCode
    | WriteSingleRegisterException ExceptionCode
    | WriteDiagnosticRegisterException ExceptionCode
    | WriteMultipleCoilsException ExceptionCode
    | WriteMultipleRegistersException ExceptionCode
    deriving (Eq, Show)

instance Serialize ModResponse where
    get = do
        fn <- getWord8
        case fn of
            0x01 -> f ReadCoilsResponse
            0x02 -> f ReadDiscreteInputsResponse
            0x03 -> f' ReadHoldingRegistersResponse
            0x04 -> f' ReadInputRegistersResponse
            0x05 -> f'' WriteSingleCoilResponse
            0x06 -> f'' WriteSingleRegisterResponse
            0x08 -> f'' WriteDiagnosticRegisterResponse
            0x0f -> f'' WriteMultipleCoilsResponse
            0x10 -> f'' WriteMultipleRegistersResponse
            0x81 -> ReadCoilsException <$> get
            0x82 -> ReadDiscreteInputsException <$> get
            0x83 -> ReadHoldingRegistersException <$> get
            0x84 -> ReadInputRegistersException <$> get
            0x85 -> WriteSingleCoilException <$> get
            0x86 -> WriteSingleRegisterException <$> get
            0x88 -> WriteDiagnosticRegisterException <$> get
            0x8f -> WriteMultipleCoilsException <$> get
            0x90 -> WriteMultipleRegistersException <$> get
            _ -> fail $ "Unsupported function code: " ++ show fn
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
        (ReadCoilsException ec) -> putWord8 0x81 >> put ec
        (ReadDiscreteInputsException ec) -> putWord8 0x82 >> put ec
        (ReadHoldingRegistersException ec) -> putWord8 0x83 >> put ec
        (ReadInputRegistersException ec) -> putWord8 0x84 >> put ec
        (WriteSingleCoilException ec) -> putWord8 0x85 >> put ec
        (WriteSingleRegisterException ec) -> putWord8 0x86 >> put ec
        (WriteDiagnosticRegisterException ec) -> putWord8 0x88 >> put ec
        (WriteMultipleCoilsException ec) -> putWord8 0x8f >> put ec
        (WriteMultipleRegistersException ec) -> putWord8 0x90 >> put ec
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
    get = do
        c <- getWord8
        case c of
          0x01 -> return IllegalFunction
          0x02 -> return IllegalDataAddress
          0x03 -> return IllegalDataValue
          0x04 -> return SlaveDeviceFailure
          0x05 -> return Acknowledge
          0x06 -> return SlaveDeviceBusy
          0x08 -> return MemoryParityError
          0x0A -> return GatewayPathUnavailable
          0x0B -> return GatewayTargetFailedToRespond
          _    -> fail $ "Unsupported exception code: " ++ show c
