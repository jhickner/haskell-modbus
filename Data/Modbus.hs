module Data.Modbus 
       ( ModTransactionResult (..)
       , AduRequest (..)
       , AduResponse (..)
       , PublicModRequest (..)
       , PublicModResponse (..)
       , ExceptionCode (..)
       , modFunction
       , retryModFunction
       , serialTransport
       , networkTransport
       , pack
       ) where

import Prelude hiding (catch)
import qualified Data.ByteString as B
import qualified System.Hardware.Serialport as SP
import Data.Attoparsec
import qualified Data.Attoparsec.Lazy as AL
import Data.Digest.CRC16
import Data.Word
import Data.Bits
import Control.Exception
import qualified Network.Socket as NS -- hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as NB
import System.Timeout
import Data.List (foldl')

{-- Haskell Modbus implementation                 
    Modbus protocol is formed from 3 layers that have archaic names:
       Transport function: Serial or TCP
       PDU: Protocol Data Unit
       ADU: Application Data Unit

    The module handles timeouts retries and exceptions, returning a
    ModTransactionResult indicating status.
--}
    

-- Result of Modbus transaction
data ModTransactionResult a = ModSuccess (AduResponse a)
                            | ModException (AduResponse a)
                            | ModTimeout
                            | ModError String
                              deriving Show
                          

-- Datatypes for ADU
data AduRequest a = AduRequest Word8 a
                  deriving Show
data AduResponse b = AduResponse Word8 b
                   | AduError String
                   deriving Show

type AduParser b = Parser (AduResponse b)           
type PduParser a = (Word8 -> Parser (a, [Word8]))

type ModTransport a b = AduRequest a -> IO (ModTransactionResult b)

-- Datatypes for PDU
data PublicModRequest = ReadDiscreteInputs ModAddress Word16
                      | ReadCoils ModAddress Word16
                      | WriteSingleCoil ModAddress Word16
                      | WriteMultipleCoils ModAddress Word16 Word8 
                      | ReadInputRegister ModAddress Word16
                      | ReadHoldingRegisters ModAddress Word16
                      | WriteSingleRegister ModAddress Word16
                      | WriteMultipleRegisters ModAddress Word16 Word8
                        deriving Show

data PublicModResponse = ReadDiscreteInputsResponse Word8 [Word8]
                       | ReadCoilsResponse Word8 [Word8]
                       | WriteSingleCoilResponse ModAddress Word16
                       | WriteMultipleCoilsResponse ModAddress Word16
                       | ReadInputRegisterResponse Word8 [Word16]
                       | ReadHoldingRegistersResponse Word8 [Word16]
                       | WriteSingleRegisterResponse ModAddress Word16
                       | WriteMultipleRegistersResponse ModAddress Word16
                       | ExceptionResponse FunctionCode ExceptionCode
                       | UnknownFunctionResponse FunctionCode
                         deriving Show

data ExceptionCode = IllegalFunction
                   | IllegalDataAddress
                   | IllegalDataValue
                   | ServerDeviceFailure
                   | Acknowledge
                   | ServerDeviceBusy
                   | MemoryParityError
                   | GatewayPathUnavailible
                   | GatewayTargetFailedToRespond
                   | UnknownExceptionCode                   
                   deriving Show
                     
-- Type aliases for doocumentation
type Retries = Integer
type ServerId = Word8
type CRC = Word16
type FunctionCode = Word8                       
type ModAddress = Word16
type ModCount = Word16
type TimeoutUs = Int

    
serialTransport :: SP.SerialPort 
                -> AduRequest PublicModRequest 
                -> IO (ModTransactionResult PublicModResponse)    
serialTransport s = modTransport (serialSend s) 
                                 (serialRecv s) 
                                 encodePublicModRequest 
                                 decodePublicModResponse

networkTransport :: NS.Socket 
                 -> AduRequest PublicModRequest 
                 -> IO (ModTransactionResult PublicModResponse)    
networkTransport s = modTransport (networkSend s) 
                                  (networkRecv s) 
                                  encodePublicModRequest 
                                  decodePublicModResponse

-- Modbus function calls

retryModFunction :: ModTransport a b 
                 -> TimeoutUs 
                 -> Retries 
                 -> AduRequest a 
                 -> IO (ModTransactionResult b)
retryModFunction _ _ 0 _ = return $ ModError "Maximum retries exceeded"
retryModFunction transport to retries adu = do
  r <- modFunction transport to adu
  case r of
    ModSuccess _ -> return r
    _            -> retryModFunction transport to (retries-1) adu


{-- Generic timout function wraps a transport function
    with a timeout.
    Tries a transaction with the transport fn and waits
    timeout interval before returning the result or
    ModTimeout
 --}
modFunction :: ModTransport a b -> TimeoutUs -> AduRequest a -> IO (ModTransactionResult b)
modFunction transport to adu = do
  mRes <- timeout to $ transport adu
  case mRes of
    Just mr -> return mr 
    Nothing -> return ModTimeout              

{-- Modbus serial or network transport drivers --}
    
{-- Generic transport function takes send and recieve fns as well    
    as an encoder and parser for the PduRequest (a) and PduResponse (b)
    types respectively. Curry this function with your favorite types to
    create an (AduRequest a) -> IO (ModTransactionResult b).
--}
modTransport :: ([Word8] -> IO ()) 
             -> IO B.ByteString 
             -> (a -> [Word8]) 
             -> PduParser b 
             -> AduRequest a 
             -> IO (ModTransactionResult b)
modTransport sendFn recvFn pduEnc pduP adu =
      catch communicate handler
      where
--      communicate :: IO (ModTransactionResult a)
        communicate = 
          let outBytes = encodeADU pduEnc adu in do
            sendFn outBytes
            bytes <- recvFn
            resultParse $ parse (aduParser pduP) bytes
        handler e = return $ ModError $ show (e::IOException) 
--      resultParse :: Result (AduResponse a) -> IO (ModTransactionResult a)
        resultParse result =
          case result of
            Fail{} -> return $ ModError "Error parsing response msg"
            Partial p -> do
              bytes <- recvFn
              resultParse $ p bytes
            Done _ adu' ->
              case adu' of 
                AduError msg -> return $ ModError msg
                _ -> return $ ModSuccess adu'

{-- Functions for the serial or network transport --}
              
-- | send a modbus request 
networkSend :: NS.Socket -> [Word8] -> IO ()
networkSend sock msg = do
    NB.send sock (B.pack msg)
    return ()

-- | retrieve the response          
networkRecv :: NS.Socket -> IO B.ByteString 
networkRecv sock = NB.recv sock 512
          
-- | send a modbus request 
serialSend :: SP.SerialPort -> [Word8] -> IO()
serialSend sp msg = do
    print $ "--> " ++ show msg
    SP.send sp $ B.pack msg
    SP.flush sp
  
-- | retrieve the response          
serialRecv :: SP.SerialPort -> IO B.ByteString
serialRecv sp = do
    bytes <- SP.recv sp 10 
    print $ "<-- " ++ show (B.unpack bytes)
    return bytes

{-- Encode an ADU by prepending server id to the PDU encoding 
    and appending the crc16.
--}
encodeADU :: (a -> [Word8]) -> AduRequest a -> [Word8]
encodeADU encoder (AduRequest id' req) = 
    let req' = encoder req 
        msg = id':req' in
    msg ++ encodeCrc16 (crc16 msg)

{-- I think there may be a different encoding for some TCP
    implementations. This might need to be implemented.
--}
--encodeTcpADU :: AduRequest a -> [Word8]
--encodeTcpADU (AduRequest id req) = undefined

{-- | attoparsec parser for modbus response packets, takes
     the PDU parser as a parameter.
--}
aduParser :: PduParser a -> AduParser a
aduParser pduParser = do
    slaveId <- anyWord8
    code <- anyWord8
    (resp, bytes) <- pduParser code
    (crc,_) <- anyCrc16
    return $ if crc == crc16 (slaveId:code:bytes)
      then AduResponse slaveId resp
      else AduError $ "CRC Error on Modbus response from: " ++ show slaveId

{-- Modbus spec defines public and privete function codes.
    This encoder supports a subset of the public codes.
    Please extend this function with missing public codes.
    If you need to support private codes create a new
    data type and encode/decode functions.
--}
encodePublicModRequest :: PublicModRequest -> [Word8]
encodePublicModRequest req = 
  case req of
    ReadDiscreteInputs addr n -> 0x02 : encodeWord16 addr ++ encodeWord16 n
    ReadCoils addr n -> 0x01 : encodeWord16 addr ++ encodeWord16 n
    WriteSingleCoil addr val -> 0x05 : encodeWord16 addr ++ encodeWord16 val
    WriteMultipleCoils addr quantity n -> 
      0x0F : encodeWord16 addr ++ encodeWord16 quantity ++ [n]
    ReadInputRegister addr _ -> 0x04 : encodeWord16 addr
    ReadHoldingRegisters addr n -> 0x3 : encodeWord16 addr ++ encodeWord16 n
    WriteSingleRegister addr val -> 0x06 : encodeWord16 addr ++ encodeWord16 val
    WriteMultipleRegisters addr quantity n -> 
      0x10 : encodeWord16 addr ++ encodeWord16 quantity ++ [n]

{-- Modbus spec defines public and privete function codes.
    This decoder supports a subset of the public codes.
    Please extend this function with missing public codes.
    If you need to support private codes create a new
    data type and encode/decode functions.
--}
decodePublicModResponse :: PduParser PublicModResponse 
                        --Word8 -> Parser (PublicModResponse, [Word8])
decodePublicModResponse code =
    case code of
      0x02 -> do 
        (n,msg) <- wordList8
        return (ReadDiscreteInputsResponse n msg, n:msg)
      0x01 -> do
        (n,msg) <- wordList8
        return (ReadCoilsResponse n msg, n:msg)
      0x05 -> do 
        (addr, addr') <- anyWord16
        (val, val') <- anyWord16
        return (WriteSingleCoilResponse addr val, addr' ++ val')
      0x0F -> do
        (addr, addr') <- anyWord16
        (cnt, cnt') <- anyWord16
        return (WriteMultipleCoilsResponse addr cnt, addr' ++ cnt')
      0x04 -> do
        (n,msg16, msg8) <- wordList16
        return (ReadInputRegisterResponse n msg16, n:msg8)
      0x03 -> do
        (n,msg16, msg8) <- wordList16
        return (ReadHoldingRegistersResponse n msg16, n:msg8)
      0x06 -> do
        (addr, addr') <- anyWord16
        (val, val') <- anyWord16
        return (WriteSingleRegisterResponse addr val, addr' ++ val')
      0x10 -> do
        (addr, addr') <- anyWord16
        (cnt, cnt') <- anyWord16
        return (WriteMultipleRegistersResponse addr cnt, addr' ++ cnt')
      fc | fc > 0x7F -> do
        ec <- anyWord8
        return (ExceptionResponse fc (decodeExceptionCode ec), [ec])
      fc -> 
        return (UnknownFunctionResponse fc, [fc])
    where
      wordList8 = do
        n <- anyWord8
        msg <- takeWord8 (fromIntegral n)
        return (n , msg)
      wordList16 = do
        n <- anyWord8
        msg <- takeWord8 (fromIntegral n)
        return (n , pack msg, msg)
      takeWord8 n = B.unpack `fmap` AL.take n

{-- Decode the exception function codes --}
decodeExceptionCode :: Word8 -> ExceptionCode
decodeExceptionCode c =
    case c of
      0x01 -> IllegalFunction
      0x02 -> IllegalDataAddress
      0x03 -> IllegalDataValue
      0x04 -> ServerDeviceFailure
      0x05 -> Acknowledge
      0x06 -> ServerDeviceBusy
      0x08 -> MemoryParityError
      0x0A -> GatewayPathUnavailible
      0x0B -> GatewayTargetFailedToRespond
      _    -> UnknownExceptionCode        

-- | Modbus encodes words MSB first
encodeWord16 :: Word16 -> [Word8]
encodeWord16 w16 = [hiByte w16, loByte w16]

-- | Crc is encoded LSB first
encodeCrc16 :: Word16 -> [Word8]
encodeCrc16 w16 = [loByte w16, hiByte w16]

-- | Modbus encodes words MSB first
anyWord16 :: Parser (Word16, [Word8])
anyWord16 = do
    h <- anyWord8
    l <- anyWord8
    return (fromIntegral h `shiftL` 8 + fromIntegral l, [h,l])

-- | Crc is encoded LSB first    
anyCrc16 :: Parser (Word16, [Word8])
anyCrc16 = do
    l <- anyWord8
    h <- anyWord8
    return ((fromIntegral h `shiftL` 8) + fromIntegral l, [h,l])
            
hiByte :: Word16 -> Word8
hiByte = fromIntegral . (`shiftR` 8)

loByte :: Word16 -> Word8
loByte = fromIntegral . (.&. 0xff)

-- | pack every 2 integrals in a list into a larger integral
-- i.e. [Word8] -> [Word16] or [Word16] -> [Word32]
pack :: (Integral a, Integral b) => [a] -> [b]
pack = map (fromIntegral . upShift) . chunk 2

upShift :: Integral a => [a] -> Int
upShift = foldl' acc 0
  where acc a o = (a `shiftL` 8) .|. fromIntegral o

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = y1 : chunk n y2 where (y1,y2) = splitAt n xs
