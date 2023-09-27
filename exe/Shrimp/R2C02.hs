module Shrimp.R2C02 (
    R2C02 (..),
    tick,
    reset,
    cpuRead,
    cpuWrite,
) where

import Control.Monad.State
import Data.Word
import Shrimp.AbstractBus

data R2C02 = R2C02

tick :: (PPUBus a) => State (R2C02, a) ()
tick = undefined -- TODO: This. Problably the hardest function so far.

--
readStatus :: (PPUBus a) => State (R2C02, a) Word8
readStatus = return 0 -- TODO: Implement Status Read support

readPPUData :: (PPUBus a) => State (R2C02, a) Word8
readPPUData = return 0 -- TODO: Implement PPU Data Read support

-- CPU Interface
cpuRead :: (PPUBus a) => Word16 -> State (R2C02, a) Word8
cpuRead addr
    | addr == 0x0000 = return 0 -- Control: Write Only
    | addr == 0x0001 = return 0 -- Mask: Write Only
    | addr == 0x0002 = readStatus
    | addr == 0x0003 = return 0 -- TODO: Implement OAS ADDR support
    | addr == 0x0004 = return 0 -- TODO: Implement OAS DATA support
    | addr == 0x0005 = return 0 -- Scroll: Write Only
    | addr == 0x0006 = return 0 -- PPUR Addr: Write Only
    | addr == 0x0007 = readPPUData
    | otherwise = return 0 -- TODO: Log error

writeControl :: (PPUBus a) => Word8 -> State (R2C02, a) ()
writeControl byte = return () -- TODO: Implement Control Write suport

writeMask :: (PPUBus a) => Word8 -> State (R2C02, a) ()
writeMask byte = return () -- TODO: Implement Mask Write support

writeScroll :: (PPUBus a) => Word8 -> State (R2C02, a) ()
writeScroll byte = return () -- TODO: Implement Scroll Write support

writePPUAddr :: (PPUBus a) => Word8 -> State (R2C02, a) ()
writePPUAddr byte = return () -- TODO: Implement PPU Address Write support

writePPUData :: (PPUBus a) => Word8 -> State (R2C02, a) ()
writePPUData byte = return () -- TODO: Implement PPU Data Write support

cpuWrite :: (PPUBus a) => Word16 -> Word8 -> State (R2C02, a) ()
cpuWrite addr byte
    | addr == 0x0000 = writeControl byte
    | addr == 0x0001 = writeMask byte
    | addr == 0x0002 = return () -- Status: Read Only
    | addr == 0x0003 = return () -- TODO: Implement OAS ADDR support
    | addr == 0x0004 = return () -- TODO: Implement OAS DATA support
    | addr == 0x0005 = writeScroll byte
    | addr == 0x0006 = writePPUAddr byte
    | addr == 0x0007 = writePPUData byte
    | otherwise = return () -- TODO: Log error

reset :: (PPUBus a) => State (R2C02, a) ()
reset = undefined
