module Shrimp.Memory where
import Data.Word

class Memory a where
    getByte :: a -> Word16 -> Word8
    putByte :: a -> Word16 -> Word8 -> a
