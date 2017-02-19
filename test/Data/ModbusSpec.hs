{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.ModbusSpec (spec) where

import Data.ByteString
import Data.Modbus
import Data.Serialize
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.DeriveTH
import Test.QuickCheck
import Test.QuickCheck.Instances ()

spec :: Spec
spec = do
  describe "ModRequest" $ do
    it "should serialize requests properly" $
      (encode <$> requests) `shouldBe` requestsEncoded
    it "should deserialize to the original modbus request" $
      (decode <$> requestsEncoded) `shouldBe` (Right <$> requests)

  describe "ModResponse" $ do
    it "should serialize responses properly" $
      (encode <$> responses) `shouldBe` responsesEncoded
    it "should deserialize to the original modbus response" $
       (decode <$> responsesEncoded) `shouldBe` (Right <$> responses)

  describe "(decode . encode) == id" $ do
    prop "ModRequest" $ \ (req :: ModRequest) -> (decode . encode) req == Right req
    -- TODO: fix ModResponse so the property holds, it's ReadCoilsResponse that's problematic
    -- prop "ModResponse" $ \ (res :: ModResponse) -> (decode . encode) res == Right res

requests :: [ModRequest]
requests = [ ReadCoils 1 1
           , ReadDiscreteInputs 1 1
           , ReadHoldingRegisters 1 1
           , ReadInputRegisters 1 1
           , WriteSingleCoil 1 1
           , WriteSingleRegister 1 1
           , WriteDiagnosticRegister 1 1
           , WriteMultipleCoils 0 [False, False, True]
           , WriteMultipleRegisters 0 [0x1234, 0x3412]
           ]

requestsEncoded :: [ByteString]
requestsEncoded = pack <$> [ [1,0,1,0,1]
                           , [2,0,1,0,1]
                           , [3,0,1,0,1]
                           , [4,0,1,0,1]
                           , [5,0,1,0,1]
                           , [6,0,1,0,1]
                           , [8,0,1,0,1]
                           , [15,0,0,0,3,1,4]
                           , [16,0,0,0,2,4,18,52, 52, 18]
                           ]

responses :: [ModResponse]
responses = [ ReadCoilsResponse [True, False, False, False, False, False, False, False]
            , ReadDiscreteInputsResponse [True, False, False, False, False, False, False, False]
            , ReadHoldingRegistersResponse [1]
            , ReadInputRegistersResponse [1]
            , WriteSingleCoilResponse 1 1
            , WriteSingleRegisterResponse 1 1
            , WriteDiagnosticRegisterResponse 1 1
            , WriteMultipleCoilsResponse 1 1
            , WriteMultipleRegistersResponse 1 1
            ] ++
            [ ReadCoilsException IllegalFunction
            , ReadCoilsException IllegalDataAddress
            , ReadCoilsException IllegalDataValue
            , ReadCoilsException SlaveDeviceFailure
            , ReadCoilsException Acknowledge
            , ReadCoilsException SlaveDeviceBusy
            , ReadCoilsException MemoryParityError
            , ReadCoilsException GatewayPathUnavailable
            , ReadCoilsException GatewayTargetFailedToRespond
            ]

responsesEncoded :: [ByteString]
responsesEncoded = pack <$> [ [1,1,1]
                            , [2,1,1]
                            , [3,2,0,1]
                            , [4,2,0,1]
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
                            ]

derive makeArbitrary ''ModRequest
derive makeArbitrary ''ModResponse
derive makeArbitrary ''ExceptionCode
