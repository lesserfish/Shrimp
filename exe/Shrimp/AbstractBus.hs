{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Shrimp.AbstractBus where

import Control.Monad.Identity
import Data.Word

class (Monad m) => CBus m a where
    cWriteByte :: Word16 -> Word8 -> a -> m a
    cReadByte :: Word16 -> a -> m (a, Word8)
    cPeek :: Word16 -> a -> m Word8

class (Monad m) => MCBus m where
    mcWriteByte :: Word16 -> Word8 -> m ()
    mcReadByte :: Word16 -> m Word8
    mcPeek :: Word16 -> m Word8

instance (MCBus m) => CBus m () where
    cWriteByte addr content _ = mcWriteByte addr content >> return ()
    cReadByte addr _ = do
        byte <- mcReadByte addr
        return (undefined, byte)
    cPeek addr _ = mcPeek addr

class PCBus a where
    pcWriteByte :: Word16 -> Word8 -> a -> a
    pcReadByte :: Word16 -> a -> (a, Word8)
    pcPeek :: Word16 -> a -> Word8

instance (PCBus a) => CBus Identity a where
    cWriteByte addr content bus = Identity $ pcWriteByte addr content bus
    cReadByte addr bus = Identity $ pcReadByte addr bus
    cPeek addr bus = Identity $ pcPeek addr bus

-- class CPUBus a where
--    cpuWriteByte :: Word16 -> Word8 -> a -> a
--    cpuReadByte :: Word16 -> a -> (a, Word8)
--    cpuPeek :: Word16 -> a -> Word8

class PPUBus a where
    ppuWriteByte :: Word16 -> Word8 -> a -> a
    ppuReadByte :: Word16 -> a -> (a, Word8)
    ppuPeek :: Word16 -> a -> Word8
    setPixel :: (Word8, Word8) -> Word8 -> a -> a
