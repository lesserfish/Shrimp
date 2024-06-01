module Shrimp.Utils where

import Text.Printf
import Data.Word
import Data.Bits


-- TODO: Check if this is affecting performance. Perhaps specifying a data type would be best. I.e. Just support Word16 and cast everything to word16?
b0 x = testBit x 0
b1 x = testBit x 1
b2 x = testBit x 2
b3 x = testBit x 3
b4 x = testBit x 4
b5 x = testBit x 5
b6 x = testBit x 6
b7 x = testBit x 7
b8 x = testBit x 8
b9 x = testBit x 9
b10 x = testBit x 10
b11 x = testBit x 11
b12 x = testBit x 12
b13 x = testBit x 13
b14 x = testBit x 14
b15 x = testBit x 15



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

toHex2 :: Word8 -> String
toHex2 w = printf "%02X" w

toHex4 :: Word16 -> String
toHex4 w = printf "%04X" w

shiftTake :: Int -> Integer -> Word8 -> Word8
shiftTake s t x = (x .>>. s) .&. (2 ^ t - 1)

shiftTake' :: Int -> Integer -> Word16 -> Word16
shiftTake' s t x = (x .>>. s) .&. (2 ^ t - 1)

