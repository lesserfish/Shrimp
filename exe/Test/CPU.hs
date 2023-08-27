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

data CPUState = CPUState
    { pc :: Word16
    , s :: Word8
    , a :: Word8
    , x :: Word8
    , y :: Word8
    , p :: Word8
    , ram :: [(Word16, Word8)]
    }
    deriving (Generic, Show, FromJSON)

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
showMem mem = show (filter (\(_, x) -> x /= 0) (assocs mem))

instance Show Barebones where
    show barebones = show (bCpu barebones) ++ "\nLog: " ++ show (bLog barebones) ++ "\nRam: " ++ showMem (bRam barebones)
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

loadBarebones :: CPUState -> Barebones
loadBarebones cpustate = Barebones (loadCPU cpustate) (loadMemory . ram $ cpustate) []

join :: (MOS6502, Barebones) -> Barebones
join (cpu, barebones) = barebones{bCpu = cpu}

tickBarebones :: Barebones -> Barebones
tickBarebones barebones = barebones'
  where
    barebones' = join result
    result = execState tick (bCpu barebones, barebones)

(.**.) :: (a -> a) -> Int -> (a -> a)
(.**.) f n x
    | n == 1 = f x
    | otherwise = f . (f .**. (n - 1)) $ x
