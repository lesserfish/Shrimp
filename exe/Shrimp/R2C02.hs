module Shrimp.R2C02 (
    R2C02 (..),
    tick,
    cpuRead,
    cpuWrite,
) where

import Control.Monad.State
import Data.Word
import Shrimp.AbstractBus

data R2C02 = R2C02

tick :: (PPUBus a) => State (R2C02, a) ()
tick = undefined -- TODO: This

-- CPU Interface
cpuRead :: (PPUBus a) => Word16 -> State (R2C02, a) Word8
cpuRead = undefined -- TODO: This

cpuWrite :: (PPUBus a) => Word16 -> Word8 -> State (R2C02, a) ()
cpuWrite = undefined -- TODO: This
