{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.CPU where

import Control.Monad.State
import Data.Aeson (FromJSON)
import Data.Array
import Data.Word
import GHC.Generics
import Shrimp.AbstractBus
import Shrimp.MOS6502
import Text.Printf

data CPUState = CPUState
    { pc :: Word16
    , s :: Word8
    , a :: Word8
    , x :: Word8
    , y :: Word8
    , p :: Word8
    , ram :: [(Word16, Word8)]
    }
    deriving (Generic, FromJSON)

data Test = Test
    { name :: String
    , initial :: CPUState
    , final :: CPUState
    , cycles :: [(Word16, Word8, String)]
    }
    deriving (Generic, Show, FromJSON)

data Barebones = Barebones
    { bCpu :: MOS6502
    , bRam :: Array Word16 Word8
    , bLog :: [(Word16, Word8, String)]
    }

showMem :: Array Word16 Word8 -> String
showMem mem = showRam (filter (\(_, x) -> x /= 0) (assocs mem))

showRam' :: [(Word16, Word8)] -> String
showRam' [] = ""
showRam' (y : ys) = (printf "(%04x , %02x)" addr value) ++ rest
  where
    (addr, value) = y
    rest = showRam' ys

showRam :: [(Word16, Word8)] -> String
showRam y = "[" ++ showRam' y ++ "]"

showLog' :: [(Word16, Word8, String)] -> String
showLog' [] = ""
showLog' (y : ys) = (printf "(%04x , %02x, " addr value) ++ str ++ ")" ++ rest
  where
    (addr, value, str) = y
    rest = showLog' ys

showLog :: [(Word16, Word8, String)] -> String
showLog y = "[" ++ showLog' y ++ "]"

instance Show CPUState where
    show cpustate =
        "CPU:"
            ++ "\npc: \t"
            ++ showWord16 (Test.CPU.pc cpustate)
            ++ "\ns: \t"
            ++ showWord8 (s cpustate)
            ++ "\na: \t"
            ++ showWord8 (a cpustate)
            ++ "\nx: \t"
            ++ showWord8 (x cpustate)
            ++ "\ny: \t"
            ++ showWord8 (y cpustate)
            ++ "\np: \t"
            ++ showWord8 (p cpustate)
            ++ "\n"
            ++ (showRam . ram $ cpustate)
            ++ "\n"

instance Show Barebones where
    show barebones =
        show (bCpu barebones)
            ++ "\nLog: "
            ++ showLog (bLog barebones)
            ++ "\nRam: "
            ++ showMem (bRam barebones)
            ++ "\nCycles: "
            ++ showLog (bLog $ barebones)

pushLog :: (Word16, Word8, String) -> Barebones -> Barebones
pushLog info barebones = barebones{bLog = (bLog barebones) ++ [info]}

instance AbstractBus Barebones where
    readByte addr barebones = (barebones', byte)
      where
        byte = (bRam barebones) ! addr
        barebones' = pushLog (addr, byte, "read") barebones
    writeByte addr byte barebones = barebones''
      where
        ram' = (bRam barebones) // [(addr, byte)]
        barebones' = barebones{bRam = ram'}
        barebones'' = pushLog (addr, byte, "write") barebones'

emptyArray :: Int -> Array Word16 Word8
emptyArray size = listArray (0, fromIntegral size - 1) (replicate (fromIntegral size) 0)

loadMemory :: [(Word16, Word8)] -> Array Word16 Word8
loadMemory [] = emptyArray (1024 * 64)
loadMemory (y : ys) = memory // memdata
  where
    memory = loadMemory ys
    memdata = [y]

loadCPU :: CPUState -> MOS6502
loadCPU (CPUState rpc rs ra rx ry rp _) = MOS6502 reg 0 0
  where
    reg = Registers rpc rs ra rx ry rp

bLoad :: CPUState -> Barebones
bLoad cpustate = Barebones (loadCPU cpustate) (loadMemory . ram $ cpustate) []

join :: (MOS6502, Barebones) -> Barebones
join (cpu, barebones) = barebones{bCpu = cpu}

bTick :: Barebones -> Barebones
bTick barebones = barebones'
  where
    barebones' = join result
    result = execState tick (bCpu barebones, barebones)

verifyRam :: Array Word16 Word8 -> [(Word16, Word8)] -> Bool
verifyRam array [] = True
verifyRam array (y : ys) = this && that
  where
    (addr, byte) = y
    byte' = array ! addr
    this = byte == byte'
    that = verifyRam array ys

verifyCPU :: MOS6502 -> CPUState -> Bool
verifyCPU mos6502 cpustate = condition
  where
    pc_condition = (Shrimp.MOS6502.pc . mosRegisters $ mos6502) == (Test.CPU.pc cpustate) -- Tests whether PC register is the same
    sp_condition = (sp . mosRegisters $ mos6502) == (s cpustate) -- Tests whether SP register is the same
    acc_condition = (acc . mosRegisters $ mos6502) == (a cpustate) -- Tests whether Accumulator register is the same
    x_condition = (idx . mosRegisters $ mos6502) == (x cpustate) -- Tests whether X register is the same
    y_condition = (idy . mosRegisters $ mos6502) == (y cpustate) -- Tests whether Y register is the same
    ps_condition = (ps . mosRegisters $ mos6502) == (p cpustate) -- Tests whether PS register is the same
    condition =
        pc_condition
            && sp_condition
            && acc_condition
            && x_condition
            && y_condition
            && ps_condition

verify :: Barebones -> CPUState -> Bool
verify barebones final_state = (verifyRam (bRam barebones) (ram final_state)) && (verifyCPU (bCpu barebones) final_state)
