module Shrimp.Mapper.Type where

import Data.Word

data Mapper = Mapper
    { cpuRMap :: Word16 -> IO Word16
    , cpuWMap :: Word16 -> IO Word16
    , ppuRMap :: Word16 -> IO Word16
    , ppuWMap :: Word16 -> IO Word16
    }

