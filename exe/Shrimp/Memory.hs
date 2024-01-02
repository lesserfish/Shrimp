module Shrimp.Memory (
    RAM (..),
    new,
    fromList,
    readByte,
    writeByte,
    noRAM,
) where

import qualified Data.Vector.Unboxed as V
import Data.Word

data RAM = RAM {rMemory :: V.Vector Word8}

instance Show RAM where
    show r = "Memory of size: " ++ (show . V.length . rMemory $ r)

new :: Word16 -> Word8 -> RAM
new size e = RAM{rMemory = V.fromList (replicate (fromIntegral size) e)}

fromList :: [Word8] -> RAM
fromList list = RAM{rMemory = V.fromList list}

readByte :: RAM -> Word16 -> Word8
readByte ram addr = (rMemory ram) V.! (fromIntegral addr)

writeByte :: RAM -> Word16 -> Word8 -> RAM
writeByte ram addr byte = ram{rMemory = new_memory}
  where
    new_memory = (rMemory ram) V.// [(fromIntegral addr, byte)]

-- Empty RAM
noRAM = new 0 0
