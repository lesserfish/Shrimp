module Shrimp.NES where

import Data.IORef
import Data.STRef
import Data.Word
import Shrimp.AbstractBus
import Shrimp.Cartridge
import Shrimp.MOS6502
import Shrimp.R2C02

data CPUSpace = CPUSpace -- Consists of the RAM accessible to the CPU
data PPUSpace = PPUSpace -- Consists of the RAM accessible to the PPU

data NES = NES
    { cpu :: MOS6502
    , ppu :: PPUSpace
    , cCartref :: Cartridge
    , cpuSpace :: CPUSpace
    , ppuSpace :: PPUSpace
    }

instance CPUBus NES

-- Complicated read/write functions

instance PPUBus NES

-- Complicate read/write functions
