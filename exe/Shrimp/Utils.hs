module Shrimp.Utils where

import Text.Printf
import Data.Word
import Data.Bits


-- TODO: Check if this is affecting performance. Perhaps specifying a data type would be best. I.e. Just support Word16 and cast everything to word16?
b0' :: Word8 -> Bool
b0' x = testBit x 0
b1' :: Word8 -> Bool
b1' x = testBit x 1
b2' :: Word8 -> Bool
b2' x = testBit x 2
b3' :: Word8 -> Bool
b3' x = testBit x 3
b4' :: Word8 -> Bool
b4' x = testBit x 4
b5' :: Word8 -> Bool
b5' x = testBit x 5
b6' :: Word8 -> Bool
b6' x = testBit x 6
b7' :: Word8 -> Bool
b7' x = testBit x 7

b0 :: Word16 -> Bool
b0 x = testBit x 0
b1 :: Word16 -> Bool
b1 x = testBit x 1
b2 :: Word16 -> Bool
b2 x = testBit x 2
b3 :: Word16 -> Bool
b3 x = testBit x 3
b4 :: Word16 -> Bool
b4 x = testBit x 4
b5 :: Word16 -> Bool
b5 x = testBit x 5
b6 :: Word16 -> Bool
b6 x = testBit x 6
b7 :: Word16 -> Bool
b7 x = testBit x 7
b8 :: Word16 -> Bool
b8 x = testBit x 8
b9 :: Word16 -> Bool
b9 x = testBit x 9
b10 :: Word16 -> Bool
b10 x = testBit x 10
b11 :: Word16 -> Bool
b11 x = testBit x 11
b12 :: Word16 -> Bool
b12 x = testBit x 12
b13 :: Word16 -> Bool
b13 x = testBit x 13
b14 :: Word16 -> Bool
b14 x = testBit x 14
b15 :: Word16 -> Bool
b15 x = testBit x 15

bi0 :: Int -> Bool
bi0 x = testBit x 0
bi1 :: Int -> Bool
bi1 x = testBit x 1
bi2 :: Int -> Bool
bi2 x = testBit x 2
bi3 :: Int -> Bool
bi3 x = testBit x 3
bi4 :: Int -> Bool
bi4 x = testBit x 4
bi5 :: Int -> Bool
bi5 x = testBit x 5
bi6 :: Int -> Bool
bi6 x = testBit x 6
bi7 :: Int -> Bool
bi7 x = testBit x 7
bi8 :: Int -> Bool
bi8 x = testBit x 8
bi9 :: Int -> Bool
bi9 x = testBit x 9
bi10 :: Int -> Bool
bi10 x = testBit x 10
bi11 :: Int -> Bool
bi11 x = testBit x 11
bi12 :: Int -> Bool
bi12 x = testBit x 12
bi13 :: Int -> Bool
bi13 x = testBit x 13
bi14 :: Int -> Bool
bi14 x = testBit x 14
bi15 :: Int -> Bool
bi15 x = testBit x 15

joinBytes :: Word8 -> Word8 -> Word16
joinBytes hb lb = (fromIntegral hb .<<. 8) .|. fromIntegral lb

splitBytes :: Word16 -> (Word8, Word8)
splitBytes byte = (hb, lb)
  where
    lb = fromIntegral (0x00FF .&. byte)
    hb = fromIntegral ((byte .>>. 8) .&. 0x00FF)

encodeBCD :: Word8 -> Word8
encodeBCD word = result
  where
    lb = word .&. 0x0F
    hb = (word .>>. 4)
    result = mod (lb + 10 * hb .&. 0xFF) 100

decodeBCD :: Word8 -> Word8
decodeBCD word = result
  where
    ld = word `mod` 10
    hd = word `div` 10
    lb = ld
    hb = (hd .<<. 4) .&. 0xF0
    result = lb + hb

toHex1 :: Word8 -> String
toHex1 w = printf "%02X" w

toHex2 :: Word16 -> String
toHex2 w = printf "%04X" w

toHex4 :: Word32 -> String
toHex4 w = printf "%08X" w

toHex8 :: Word64 -> String
toHex8 w = printf "%016X" w



shiftTake1 :: Int -> Integer -> Word8 -> Word8
shiftTake1 s t x = (x .>>. s) .&. (2 ^ t - 1)

shiftTake2 :: Int -> Integer -> Word16 -> Word16
shiftTake2 s t x = (x .>>. s) .&. (2 ^ t - 1)

shiftTake4 :: Int -> Integer -> Word32 -> Word32
shiftTake4 s t x = (x .>>. s) .&. (2 ^ t - 1)

shiftTake8 :: Int -> Integer -> Word64 -> Word64
shiftTake8 s t x = (x .>>. s) .&. (2 ^ t - 1)


takeShift1 :: Integer -> Int -> Word8 -> Word8
takeShift1 t s x = (x .&. (2 ^ t - 1)) .<<. s

takeShift2 :: Integer -> Int -> Word16 -> Word16
takeShift2 t s x = (x .&. (2 ^ t - 1)) .<<. s

takeShift4 :: Integer -> Int -> Word32 -> Word32
takeShift4 t s x = (x .&. (2 ^ t - 1)) .<<. s

takeShift8 :: Integer -> Int -> Word64 -> Word64
takeShift8 t s x = (x .&. (2 ^ t - 1)) .<<. s

