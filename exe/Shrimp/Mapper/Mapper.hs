{-# LANGUAGE GADTs #-}

module Shrimp.Mapper.Mapper where

import Shrimp.Mapper.AbstractMapper

data Mapper = forall a. (AbstractMapper a) => Mapper {mapper :: a}

instance AbstractMapper Mapper where
    cpuRMap (Mapper m) addr = (Mapper m', addr')
      where
        (m', addr') = cpuRMap m addr
    cpuWMap (Mapper m) addr = (Mapper m', addr')
      where
        (m', addr') = cpuWMap m addr
    ppuRMap (Mapper m) addr = (Mapper m', addr')
      where
        (m', addr') = ppuRMap m addr
    ppuWMap (Mapper m) addr = (Mapper m', addr')
      where
        (m', addr') = ppuWMap m addr
    reset (Mapper m) = Mapper m'
      where
        m' = reset m
