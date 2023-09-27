{-# LANGUAGE GADTs #-}

module Shrimp.Mapper.Mapper where

import Shrimp.INES
import Shrimp.Mapper.AbstractMapper
import Shrimp.Mapper.Mapper0

data Mapper = forall a. (Show a, AbstractMapper a) => Mapper {mapper :: a}

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

instance Show Mapper where
    show (Mapper m) = show m

chooseMapper :: Header -> Mapper
chooseMapper header
    | (hMapper header) == 0 = Mapper (Mapper0{prgBanks0 = (hPrgSize header), chrBanks0 = (hChrSize header)})
    | otherwise = error ("Unsupported Mapper: " ++ show (hMapper header))
