{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Shrimp.AbstractBus where

import Control.Monad.Identity
import Data.Word

-- CPU

class (Monad m) => CBus m a where
    cWriteByte :: Word16 -> Word8 -> a -> m a
    cReadByte :: Word16 -> a -> m (a, Word8)
    cPeek :: Word16 -> a -> m Word8
    cDebug :: String -> a -> m a

class PCBus a where
    pcWriteByte :: Word16 -> Word8 -> a -> a
    pcReadByte :: Word16 -> a -> (a, Word8)
    pcPeek :: Word16 -> a -> Word8
    pcDebug :: String -> a -> a

instance (PCBus a) => CBus Identity a where
    cWriteByte addr content bus = Identity $ pcWriteByte addr content bus
    cReadByte addr bus = Identity $ pcReadByte addr bus
    cPeek addr bus = Identity $ pcPeek addr bus
    cDebug log a = Identity $ pcDebug log a

-- PPU

class (Monad m) => PBus m a where
    pWriteByte :: Word16 -> Word8 -> a -> m a
    pReadByte :: Word16 -> a -> m (a, Word8)
    pPeek :: Word16 -> a -> m Word8
    pSetPixel :: (Word8, Word8) -> Word8 -> a -> m a
    pDebug :: String -> a -> m a

class PPBus a where
    ppWriteByte :: Word16 -> Word8 -> a -> a
    ppReadByte :: Word16 -> a -> (a, Word8)
    ppPeek :: Word16 -> a -> Word8
    ppSetPixel :: (Word8, Word8) -> Word8 -> a -> a
    ppDebug :: String -> a -> a

instance (PPBus a) => PBus Identity a where
    pWriteByte addr content bus = Identity $ ppWriteByte addr content bus
    pReadByte addr bus = Identity $ ppReadByte addr bus
    pPeek addr bus = Identity $ ppPeek addr bus
    pSetPixel pos col bus = Identity $ ppSetPixel pos col bus
    pDebug string a = Identity $ ppDebug string a
