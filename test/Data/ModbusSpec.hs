{-# LANGUAGE OverloadedStrings #-}
module Data.ModbusSpec (main, spec) where

import Test.Hspec
import Data.Modbus
import Data.Serialize
import Control.Applicative
import Data.Word
import Data.ByteString
import Data.Either
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "singleEncode" $ do
    it "should check serialization of requests to make sure therea aren't breaking changes" $ do
      singleEncode`shouldBe` singleEncodeResult
  describe "singleDecode" $ do
    it "should Decode to a list of modbus requests of the same length" $ do
       lefts (singleDecode) `shouldBe` []
  describe "singleEncodeResponse" $ do
    it "should check serialization of responses to make sure therea aren't breaking changes" $ do
      singleEncodeResponse `shouldBe` singleEncodeResponseResult
  describe "singleResponseDecode" $ do
    it "should Decode to a list of modbus responses of the same length" $ do
       lefts(singleDecodeResponse) `shouldBe` []


-- | Generate test mod requests
testModRequestEncode :: SlaveId -> ModRegister -> Word16 -> ByteString -> [ModRequest]
testModRequestEncode sid modreg val lst = tAllRequests <*> [modreg] <*> [val] --tEncodeRequests
    where
      tReadCoils               r v = ReadCoils                r v
      tReadDiscreteInputs      r v = ReadDiscreteInputs       r v
      tReadHoldingRegisters    r v = ReadHoldingRegisters     r v
      tReadInputRegisters      r v = ReadInputRegisters       r v
      tWriteSingleCoil         r v = WriteSingleCoil          r v
      tWriteSingleRegister     r v = WriteSingleRegister      r v
      tWriteDiagnosticRegister r v = WriteDiagnosticRegister  v v
      tWriteMultipleCoils      r v = WriteMultipleCoils       1 0 0 "1111"
      tWriteMultipleRegisters  r v = WriteMultipleRegisters   1 2 1 "1111"
      tAllRequests :: [ModRegister -> Word16 -> ModRequest]
      tAllRequests = [ tReadCoils
                       ,tReadDiscreteInputs
                       ,tReadHoldingRegisters
                       ,tReadInputRegisters
                       ,tWriteSingleCoil
                       ,tWriteSingleRegister
                       ,tWriteDiagnosticRegister
                       ,tWriteMultipleCoils
                       ,tWriteMultipleRegisters
                     ]


testOneWriteMultipleRegs = WriteMultipleRegisters   1 2 1 "1111"

testDecodeOneWriteMultipleRegs :: Either String ModRequest
testDecodeOneWriteMultipleRegs = decode . encode $ testOneWriteMultipleRegs
-- A few static checks to make sure there haven't been changes in the way ceral encodes and decodes
singleEncode = encode <$> testModRequestEncode 1 1 1 "1"
singleEncodeResult = ["\SOH\NUL\SOH\NUL\SOH","\STX\NUL\SOH\NUL\SOH","\ETX\NUL\SOH\NUL\SOH","\EOT\NUL\SOH\NUL\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\NUL\NUL1111","\DLE\NUL\SOH\NUL\STX\SOH1111"]
-- |make sure the process works in reverse
testModRequestDecode :: ByteString -> Either String ModRequest
testModRequestDecode bs = runGet get bs

singleDecode = testModRequestDecode <$> singleEncodeResult

-- | Generate test mod responses

-- |Static
singleEncodeResponse = encode <$> testModResponseAllExceptions 1 1 1 1
singleEncodeResponseResult :: [ByteString]
singleEncodeResponseResult = ["\SOH\SOH\SOH","\STX\SOH\SOH","\ETX\SOH\SOH","\EOT\SOH\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\SOH","\DLE\NUL\SOH\NUL\SOH","\129\SOH","\SOH\SOH\SOH","\STX\SOH\SOH","\ETX\SOH\SOH","\EOT\SOH\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\SOH","\DLE\NUL\SOH\NUL\SOH","\129\STX","\SOH\SOH\SOH","\STX\SOH\SOH","\ETX\SOH\SOH","\EOT\SOH\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\SOH","\DLE\NUL\SOH\NUL\SOH","\129\ETX","\SOH\SOH\SOH","\STX\SOH\SOH","\ETX\SOH\SOH","\EOT\SOH\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\SOH","\DLE\NUL\SOH\NUL\SOH","\129\EOT","\SOH\SOH\SOH","\STX\SOH\SOH","\ETX\SOH\SOH","\EOT\SOH\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\SOH","\DLE\NUL\SOH\NUL\SOH","\129\ENQ","\SOH\SOH\SOH","\STX\SOH\SOH","\ETX\SOH\SOH","\EOT\SOH\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\SOH","\DLE\NUL\SOH\NUL\SOH","\129\ACK","\SOH\SOH\SOH","\STX\SOH\SOH","\ETX\SOH\SOH","\EOT\SOH\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\SOH","\DLE\NUL\SOH\NUL\SOH","\129\b","\SOH\SOH\SOH","\STX\SOH\SOH","\ETX\SOH\SOH","\EOT\SOH\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\SOH","\DLE\NUL\SOH\NUL\SOH","\129\n","\SOH\SOH\SOH","\STX\SOH\SOH","\ETX\SOH\SOH","\EOT\SOH\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\SOH","\DLE\NUL\SOH\NUL\SOH","\129\v","\SOH\SOH\SOH","\STX\SOH\SOH","\ETX\SOH\SOH","\EOT\SOH\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\SOH","\DLE\NUL\SOH\NUL\SOH","\129\255"]

singleDecodeResponse = testModResponseDecode <$> singleEncodeResponseResult

testModResponseEncode :: SlaveId -> Word8  -> Word16-> FunctionCode -> ExceptionCode -> [ModResponse]
testModResponseEncode sid adr  wd fc ec = tAllResponses <*> [adr] <*> [wd]
    where
      tReadCoilsResponse               r v =  ReadCoilsResponse            r (pack [r])
      tReadDiscreteInputsResponse      r v =  ReadDiscreteInputsResponse   r (pack [r])
      tReadHoldingRegistersResponse    r v =  ReadHoldingRegistersResponse r (pack [r])
      tReadInputRegistersResponse      r v =  ReadInputRegistersResponse   r (pack [r])
      tWriteSingleCoilResponse         r v =  WriteSingleCoilResponse      v v
      tWriteSingleRegisterResponse     r v =  WriteSingleRegisterResponse  v v
      tWriteDiagnosticRegisterResponse r v =  WriteDiagnosticRegisterResponse v v
      tWriteMultipleCoilsResponse      r v =  WriteMultipleCoilsResponse v v
      tWriteMultipleRegistersResponse  r v =  WriteMultipleRegistersResponse v v
      tExceptionResponse               r v =  ExceptionResponse  fc ec
--      tUnknownFunctionResponse         r v =  UnknownFunctionResponse 0xFF
      tAllResponses :: [Word8 -> Word16 -> ModResponse]
      tAllResponses = [tReadCoilsResponse
                      , tReadDiscreteInputsResponse
                      , tReadHoldingRegistersResponse
                      , tReadInputRegistersResponse
                      , tWriteSingleCoilResponse
                      , tWriteSingleRegisterResponse
                      , tWriteDiagnosticRegisterResponse
                      , tWriteMultipleCoilsResponse
                      , tWriteMultipleRegistersResponse
                      , tExceptionResponse ]
--                      , tUnknownFunctionResponse ]



testModResponseDecode :: ByteString -> Either String ModResponse
testModResponseDecode bs = runGet get bs

testModResponseAllExceptions :: SlaveId -> Word8 -> Word16 -> FunctionCode -> [ModResponse]
testModResponseAllExceptions sid adr wd fc = Prelude.concat $ (testModResponseEncode sid adr wd fc) <$> ecs
    where
      ecs :: [ExceptionCode]
      ecs =  [IllegalFunction
           , IllegalDataAddress
           , IllegalDataValue
           , SlaveDeviceFailure
           , Acknowledge
           , SlaveDeviceBusy
           , MemoryParityError
           , GatewayPathUnavailable
           , GatewayTargetFailedToRespond
           , UnknownExceptionCode 0xFF]

testDecodeUnknownExceptionCode :: Either String ExceptionCode
testDecodeUnknownExceptionCode = decode.encode . UnknownExceptionCode $ 0xFF
