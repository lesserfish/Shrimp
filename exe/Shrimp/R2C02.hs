module Shrimp.R2C02 (
    Context (..),
    Registers (..),
    Interface (..),
    R2C02 (..),
    Sprite (..),
    getSprite,
    new, 
    reset,
    tick,
    tick',
    cpuPeek,
    cpuWrite,
    dmaPort,
    cpuRead
) where

import Shrimp.R2C02.Internal (
    Context(..), 
    Registers(..),
    Interface(..),
    R2C02(..),
    Sprite(..),
    getSprite,
    new,
    reset,
    cpuPeek,
    cpuWrite,
    dmaPort,
    cpuRead
    )

import Shrimp.R2C02.Fast (
    tick,
    tick'
    )
