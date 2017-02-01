module Data.ModbusSpec (spec) where

import Data.ByteString
import Data.Either
import Data.Modbus
import Data.Serialize
import Data.Word
import Test.Hspec

spec :: Spec
spec = do
  describe "singleEncode" $
    it "should check serialization of requests to make sure there aren't breaking changes" $
    singleEncode`shouldBe` singleEncodeResult
  describe "singleDecode" $
    it "should Decode to a list of modbus requests of the same length" $
    lefts singleDecode `shouldBe` []
  describe "singleEncodeResponse" $
    it "should check serialization of responses to make sure there aren't breaking changes" $
    singleEncodeResponse `shouldBe` singleEncodeResponseResult
  describe "singleResponseDecode" $
    it "should Decode to a list of modbus responses of the same length" $
    lefts singleDecodeResponse `shouldBe` []

testModRequestEncode :: ModRegister -> Word16 -> ByteString -> [ModRequest]
testModRequestEncode modreg val _lst = tAllRequests <*> [modreg] <*> [val]
  where
    tAllRequests = [ ReadCoils
                   , ReadDiscreteInputs
                   , ReadHoldingRegisters
                   , ReadInputRegisters
                   , WriteSingleCoil
                   , WriteSingleRegister
                   , \ _ v -> WriteDiagnosticRegister v v
                   , \ _ _ -> WriteMultipleCoils 1 0 0 (pack $ Prelude.replicate 4 49)
                   , \ _ _ -> WriteMultipleRegisters 1 2 1 (pack $ Prelude.replicate 4 49)
                   ]

singleEncode :: [ByteString]
singleEncode = encode <$> testModRequestEncode 1 1 (pack [49])

singleEncodeResult :: [ByteString]
singleEncodeResult = pack <$> [ [1,0,1,0,1]
                              , [2,0,1,0,1]
                              , [3,0,1,0,1]
                              , [4,0,1,0,1]
                              , [5,0,1,0,1]
                              , [6,0,1,0,1]
                              , [8,0,1,0,1]
                              , [15,0,1,0,0,0,49,49,49,49]
                              , [16,0,1,0,2,1,49,49,49,49]
                              ]

singleDecode :: [Either String ModRequest]
singleDecode = decode <$> singleEncodeResult

singleEncodeResponse :: [ByteString]
singleEncodeResponse = encode <$> testModResponseAllFuncs 1 1 ++ testModResponseAllExceptions 1

singleEncodeResponseResult :: [ByteString]
singleEncodeResponseResult = fmap pack [ [1,1,1]
                                       , [2,1,1]
                                       , [3,1,1]
                                       , [4,1,1]
                                       , [5,0,1,0,1]
                                       , [6,0,1,0,1]
                                       , [8,0,1,0,1]
                                       , [15,0,1,0,1]
                                       , [16,0,1,0,1]
                                       , [129,1]
                                       , [129,2]
                                       , [129,3]
                                       , [129,4]
                                       , [129,5]
                                       , [129,6]
                                       , [129,8]
                                       , [129,10]
                                       , [129,11]
                                       , [129,255]
                                       ]

singleDecodeResponse :: [Either String ModResponse]
singleDecodeResponse = decode <$> singleEncodeResponseResult

testModResponseAllFuncs :: Word8  -> Word16-> [ModResponse]
testModResponseAllFuncs adr  wd = tAllResponses <*> [adr] <*> [wd]
    where
      tAllResponses = [ \ r _ -> ReadCoilsResponse r (pack [r])
                      , \ r _ -> ReadDiscreteInputsResponse r (pack [r])
                      , \ r _ -> ReadHoldingRegistersResponse r (pack [r])
                      , \ r _ -> ReadInputRegistersResponse   r (pack [r])
                      , \ _ v -> WriteSingleCoilResponse      v v
                      , \ _ v -> WriteSingleRegisterResponse  v v
                      , \ _ v -> WriteDiagnosticRegisterResponse v v
                      , \ _ v -> WriteMultipleCoilsResponse v v
                      , \ _ v -> WriteMultipleRegistersResponse v v
                      ]

testModResponseAllExceptions :: FunctionCode -> [ModResponse]
testModResponseAllExceptions fc = ExceptionResponse fc <$> ecs
    where
      ecs =  [ IllegalFunction
             , IllegalDataAddress
             , IllegalDataValue
             , SlaveDeviceFailure
             , Acknowledge
             , SlaveDeviceBusy
             , MemoryParityError
             , GatewayPathUnavailable
             , GatewayTargetFailedToRespond
             , UnknownExceptionCode 0xFF]
