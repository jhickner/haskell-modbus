{-# LANGUAGE OverloadedStrings #-}
module Data.ModbusSpec (main, spec) where

import Test.Hspec
import Data.Modbus
import Data.Serialize 
import Control.Applicative
import Data.Word
import Data.ByteString
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "singleEncode" $ do
    it "should check serialization of requests to make sure therea aren't breaking changes" $ do
      singlEncode`shouldBe` singleEncodeResult
  describe "singleDecode" $ do 
    it "should Decode to a list of modbus requests of the same length" $ do
       Prelude.length (singleDecode) `shouldBe` 9


-- | Generate test mod requests
testModRequestFramesEncode :: SlaveId -> ModRegister -> Word16 -> ByteString -> [ModRequest]
testModRequestFramesEncode sid modreg val lst = tAllRequests <*> [modreg] <*> [val] --tEncodeRequests
    where
      tReadCoils               r v = ReadCoils                r v
      tReadDiscreteInputs      r v = ReadDiscreteInputs       r v
      tReadHoldingRegisters    r v = ReadHoldingRegisters     r v
      tReadInputRegisters      r v = ReadInputRegisters       r v
      tWriteSingleCoil         r v = WriteSingleCoil          r v
      tWriteSingleRegister     r v = WriteSingleRegister      r v
      tWriteDiagnosticRegister r v = WriteDiagnosticRegister  v v
      tWriteMultipleCoils      r v = WriteMultipleCoils       r v 3 lst
      tWriteMultipleRegisters  r v = WriteMultipleRegisters   r v 3 lst
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

-- A few static checks to make sure there haven't been changes in the way ceral encodes and decodes
singlEncode = encode $ testModRequestFramesEncode 1 1 1 "1"
singleEncodeResult = "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\SOH\NUL\SOH\NUL\SOH\STX\NUL\SOH\NUL\SOH\ETX\NUL\SOH\NUL\SOH\EOT\NUL\SOH\NUL\SOH\ENQ\NUL\SOH\NUL\SOH\ACK\NUL\SOH\NUL\SOH\b\NUL\SOH\NUL\SOH\SI\NUL\SOH\NUL\SOH\ETX1\DLE\NUL\SOH\NUL\SOH\ETX1"



-- |make sure the process works in reverse
testModRequestFramesDecode :: ByteString -> [ModRequest] 
testModRequestFramesDecode bs = case runGet get bs of 
                                  Left e -> []
                                  Right r -> r

singleDecode = testModRequestFramesDecode singleEncodeResult 
singleDecodeResult = testModRequestFramesEncode 1 1 1 "1"




-- | Generate test mod responses 
{-|
ReadCoilsResponse                Word8         ByteString
ReadDiscreteInputsResponse       Word8         ByteString
ReadHoldingRegistersResponse     Word8         ByteString
ReadInputRegistersResponse       Word8         ByteString
WriteSingleCoilResponse          ModRegister   Word16
WriteSingleRegisterResponse      ModRegister   Word16
WriteDiagnosticRegisterResponse  Word16        Word16
WriteMultipleCoilsResponse       ModRegister   Word16
WriteMultipleRegistersResponse   ModRegister   Word16
ExceptionResponse                FunctionCode  ExceptionCode
UnknownFunctionResponse          FunctionCode
|-}




tReadCoilsResponse               r v =  ReadCoilsResponse                
tReadDiscreteInputsResponse      r v =  ReadDiscreteInputsResponse       
tReadHoldingRegistersResponse    r v =  ReadHoldingRegistersResponse     
tReadInputRegistersResponse      r v =  ReadInputRegistersResponse       
tWriteSingleCoilResponse         r v =  WriteSingleCoilResponse          
tWriteSingleRegisterResponse     r v =  WriteSingleRegisterResponse      
tWriteDiagnosticRegisterResponse r v =  WriteDiagnosticRegisterResponse  
tWriteMultipleCoilsResponse      r v =  WriteMultipleCoilsResponse       
tWriteMultipleRegistersResponse  r v =  WriteMultipleRegistersResponse   
tExceptionResponse               r v =  ExceptionResponse                
tUnknownFunctionResponse         r v =  UnknownFunctionResponse          
