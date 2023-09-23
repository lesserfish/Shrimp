{-# LANGUAGE GADTs #-}

module Shrimp.Mapper.AbstractMapper where

import Data.Word

class AbstractMapper a where
    cpuRMap :: a -> Word16 -> (a, Word16)
    cpuWMap :: a -> Word16 -> (a, Word16)
    ppuRMap :: a -> Word16 -> (a, Word16)
    ppuWMap :: a -> Word16 -> (a, Word16)
    reset :: a -> a
