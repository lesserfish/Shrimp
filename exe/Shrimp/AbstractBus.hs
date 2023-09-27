module Shrimp.AbstractBus where

import Data.Word

class CPUBus a where
    cpuWriteByte :: Word16 -> Word8 -> a -> a
    cpuReadByte :: Word16 -> a -> (a, Word8)
    cpuPeek :: Word16 -> a -> Word8

class PPUBus a where
    ppuWriteByte :: Word16 -> Word8 -> a -> a
    ppuReadByte :: Word16 -> a -> (a, Word8)
    ppuPeek :: Word16 -> a -> Word8
    setPixel :: (Word8, Word8) -> Word8 -> a -> a
