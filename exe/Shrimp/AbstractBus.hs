module Shrimp.AbstractBus where

import Data.Word

class CPUBus a where
    cpuWriteByte :: Word16 -> Word8 -> a -> a
    cpuReadByte :: Word16 -> a -> (a, Word8)
    cpuPeek :: Word16 -> a -> Word8

class PPUBus a where
    writeByte :: Word16 -> Word8 -> a -> a
    readByte :: Word16 -> a -> (a, Word8)
    peek :: Word16 -> a -> Word8
