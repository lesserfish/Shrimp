module Shrimp.Mapper.Mapper0 where

import Data.Bits
import Data.Word
import Shrimp.Mapper.AbstractMapper

data Mapper0 = Mapper0 {prgBanks0 :: Word8, chrBanks0 :: Word8}

instance AbstractMapper Mapper0 where
    cpuRMap m addr
        | prgBanks0 m > 1 = (m, shifted_addr)
        | otherwise = (m, shifted_addr')
      where
        shifted_addr = addr .&. 0x7FFF
        shifted_addr' = addr .&. 0x3FFF
    cpuWMap m addr
        | prgBanks0 m > 1 = (m, shifted_addr)
        | otherwise = (m, shifted_addr')
      where
        shifted_addr = addr .&. 0x7FFF
        shifted_addr' = addr .&. 0x3FFF
    ppuRMap m addr = (m, addr)
    ppuWMap m addr = (m, addr)
    reset m = m
