module Shrimp.R2C02 (
    R2C02 (..),
    r2c02,
    tick,
    reset,
    cpuRead,
    cpuWrite,
) where

import Control.Monad.State
import Data.Bits
import Data.Word
import Shrimp.AbstractBus
import qualified Shrimp.Memory as Memory

data R2C02 = R2C02 {}

r2c02 :: R2C02
r2c02 = R2C02

--
readStatus :: (PBus m a) => StateT (R2C02, a) m Word8
readStatus = return 0 -- TODO: Implement Status Read support

readPPUData :: (PBus m a) => StateT (R2C02, a) m Word8
readPPUData = return 0 -- TODO: Implement PPU Data Read support

-- CPU Interface
cpuRead :: (PBus m a) => Word16 -> StateT (R2C02, a) m Word8
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

writeControl :: (PBus m a) => Word8 -> StateT (R2C02, a) m ()
writeControl byte = return () -- TODO: Implement Control Write suport

writeMask :: (PBus m a) => Word8 -> StateT (R2C02, a) m ()
writeMask byte = return () -- TODO: Implement Mask Write support

writeScroll :: (PBus m a) => Word8 -> StateT (R2C02, a) m ()
writeScroll byte = return () -- TODO: Implement Scroll Write support

writePPUAddr :: (PBus m a) => Word8 -> StateT (R2C02, a) m ()
writePPUAddr byte = return () -- TODO: Implement PPU Address Write support

writePPUData :: (PBus m a) => Word8 -> StateT (R2C02, a) m ()
writePPUData byte = return () -- TODO: Implement PPU Data Write support

cpuWrite :: (PBus m a) => Word16 -> Word8 -> StateT (R2C02, a) m ()
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

reset :: (PBus m a) => StateT (R2C02, a) m ()
reset = do
    (r2c02, bus) <- get
    let r2c02' = r2c02
    put (r2c02', bus)

tick :: (PBus m a) => StateT (R2C02, a) m ()
tick = do
    -- TODO: This. Problably the hardest function so far.
    return ()
