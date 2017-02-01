{-# LANGUAGE OverloadedStrings #-}
module Data.ModbusSpec (spec) where

import Test.Hspec
import Data.Modbus
import Data.Serialize
import Data.Word
import Data.ByteString
import Data.Either

spec :: Spec
spec = do
  describe "singleEncode" $
    it "should check serialization of requests to make sure therea aren't breaking changes" $
      singleEncode`shouldBe` singleEncodeResult
  describe "singleDecode" $
    it "should Decode to a list of modbus requests of the same length" $
       lefts singleDecode `shouldBe` []
  describe "singleEncodeResponse" $
    it "should check serialization of responses to make sure therea aren't breaking changes" $
      singleEncodeResponse `shouldBe` singleEncodeResponseResult
  describe "singleResponseDecode" $
    it "should Decode to a list of modbus responses of the same length" $
       lefts singleDecodeResponse `shouldBe` []


-- | Generate test mod requests
testModRequestEncode :: SlaveId -> ModRegister -> Word16 -> ByteString -> [ModRequest]
testModRequestEncode _sid modreg val _lst = tAllRequests <*> [modreg] <*> [val] --tEncodeRequests
    where
      tReadCoils                   = ReadCoils
      tReadDiscreteInputs          = ReadDiscreteInputs
      tReadHoldingRegisters        = ReadHoldingRegisters
      tReadInputRegisters          = ReadInputRegisters
      tWriteSingleCoil             = WriteSingleCoil
      tWriteSingleRegister         = WriteSingleRegister
      tWriteDiagnosticRegister _ v = WriteDiagnosticRegister  v v
      tWriteMultipleCoils      _ _ = WriteMultipleCoils       1 0 0 "1111"
      tWriteMultipleRegisters  _ _ = WriteMultipleRegisters   1 2 1 "1111"
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


-- testOneWriteMultipleRegs :: ModRequest
-- testOneWriteMultipleRegs = WriteMultipleRegisters   1 2 1 "1111"

-- testDecodeOneWriteMultipleRegs :: Either String ModRequest
-- testDecodeOneWriteMultipleRegs = decode . encode $ testOneWriteMultipleRegs
-- A few static checks to make sure there haven't been changes in the way ceral encodes and decodes
singleEncode :: [ByteString]
singleEncode = encode <$> testModRequestEncode 1 1 1 "1"
singleEncodeResult :: [ByteString]
singleEncodeResult = ["\SOH\NUL\SOH\NUL\SOH","\STX\NUL\SOH\NUL\SOH","\ETX\NUL\SOH\NUL\SOH","\EOT\NUL\SOH\NUL\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\NUL\NUL1111","\DLE\NUL\SOH\NUL\STX\SOH1111"]
-- |make sure the process works in reverse
testModRequestDecode :: ByteString -> Either String ModRequest
testModRequestDecode = runGet get

singleDecode :: [Either String ModRequest]
singleDecode = testModRequestDecode <$> singleEncodeResult

-- | Generate test mod responses

-- |Static
singleEncodeResponse :: [ByteString]
singleEncodeResponse = encode <$> testModResponseAllExceptions 1 1 1 1
singleEncodeResponseResult :: [ByteString]
singleEncodeResponseResult = ["\SOH\SOH\SOH","\STX\SOH\SOH","\ETX\SOH\SOH","\EOT\SOH\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\SOH","\DLE\NUL\SOH\NUL\SOH","\129\SOH","\SOH\SOH\SOH","\STX\SOH\SOH","\ETX\SOH\SOH","\EOT\SOH\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\SOH","\DLE\NUL\SOH\NUL\SOH","\129\STX","\SOH\SOH\SOH","\STX\SOH\SOH","\ETX\SOH\SOH","\EOT\SOH\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\SOH","\DLE\NUL\SOH\NUL\SOH","\129\ETX","\SOH\SOH\SOH","\STX\SOH\SOH","\ETX\SOH\SOH","\EOT\SOH\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\SOH","\DLE\NUL\SOH\NUL\SOH","\129\EOT","\SOH\SOH\SOH","\STX\SOH\SOH","\ETX\SOH\SOH","\EOT\SOH\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\SOH","\DLE\NUL\SOH\NUL\SOH","\129\ENQ","\SOH\SOH\SOH","\STX\SOH\SOH","\ETX\SOH\SOH","\EOT\SOH\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\SOH","\DLE\NUL\SOH\NUL\SOH","\129\ACK","\SOH\SOH\SOH","\STX\SOH\SOH","\ETX\SOH\SOH","\EOT\SOH\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\SOH","\DLE\NUL\SOH\NUL\SOH","\129\b","\SOH\SOH\SOH","\STX\SOH\SOH","\ETX\SOH\SOH","\EOT\SOH\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\SOH","\DLE\NUL\SOH\NUL\SOH","\129\n","\SOH\SOH\SOH","\STX\SOH\SOH","\ETX\SOH\SOH","\EOT\SOH\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\SOH","\DLE\NUL\SOH\NUL\SOH","\129\v","\SOH\SOH\SOH","\STX\SOH\SOH","\ETX\SOH\SOH","\EOT\SOH\SOH","\ENQ\NUL\SOH\NUL\SOH","\ACK\NUL\SOH\NUL\SOH","\b\NUL\SOH\NUL\SOH","\SI\NUL\SOH\NUL\SOH","\DLE\NUL\SOH\NUL\SOH","\129\255"]

singleDecodeResponse :: [Either String ModResponse]
singleDecodeResponse = testModResponseDecode <$> singleEncodeResponseResult

testModResponseEncode :: SlaveId -> Word8  -> Word16-> FunctionCode -> ExceptionCode -> [ModResponse]
testModResponseEncode _ adr  wd fc ec = tAllResponses <*> [adr] <*> [wd]
    where
      tReadCoilsResponse               r _ =  ReadCoilsResponse            r (pack [r])
      tReadDiscreteInputsResponse      r _ =  ReadDiscreteInputsResponse   r (pack [r])
      tReadHoldingRegistersResponse    r _ =  ReadHoldingRegistersResponse r (pack [r])
      tReadInputRegistersResponse      r _ =  ReadInputRegistersResponse   r (pack [r])
      tWriteSingleCoilResponse         _ v =  WriteSingleCoilResponse      v v
      tWriteSingleRegisterResponse     _ v =  WriteSingleRegisterResponse  v v
      tWriteDiagnosticRegisterResponse _ v =  WriteDiagnosticRegisterResponse v v
      tWriteMultipleCoilsResponse      _ v =  WriteMultipleCoilsResponse v v
      tWriteMultipleRegistersResponse  _ v =  WriteMultipleRegistersResponse v v
      tExceptionResponse               _ _ =  ExceptionResponse  fc ec
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
testModResponseDecode = runGet get

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

-- testDecodeUnknownExceptionCode :: Either String ExceptionCode
-- testDecodeUnknownExceptionCode = decode.encode . UnknownExceptionCode $ 0xFF
