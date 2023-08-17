{-# LANGUAGE ExistentialQuantification #-}

module Shrimp.CPU where
import Shrimp.Memory
import Data.Word
import Control.Monad.State

data Reg = Reg { pc :: Word16   -- Program Counter
               , sp :: Word8    -- Stack Pointer
               , acc :: Word8   -- Accumulator
               , idx :: Word8   -- Index Register X
               , idy :: Word8   -- Index Register Y
               , ps :: Word8    -- Processor Status
               }

data Flag = CARRY
          | ZERO
          | INTERRUPT_DISABLE
          | DECIMAL_MODE
          | BREAK_CMD
          | OVERFLOW
          | NEGATIVE

data ADDR_MODE = IMPLICIT
               | ACCUMULATOR
               | ZERO_PAGE
               | ZERO_PAGE_X
               | ZERO_PAGE_Y
               | RELATIVE
               | ABSOLUTE
               | ABSOLUTE_X
               | ABSOLUTE_Y
               | INDIRECT
               | IDX_INDIRECT
               | INDIRECT_IDX

data MOS6502 a = Memory a => MOS6502 { 
                                        cRegisters :: Reg,
                                        cMemory :: a
                                     }
                            
type CPUMonad a = State (MOS6502 a)

getFlag :: Flag -> Word8 -> Bool
getFlag = undefined
            
