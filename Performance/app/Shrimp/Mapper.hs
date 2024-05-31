{-# LANGUAGE GADTs #-}

module Shrimp.Mapper (chooseMapper, Mapper(..), noMapper) where

import Shrimp.Mapper.Type
import Shrimp.Mapper.Mapper0
import Shrimp.Cartridge.Loader


chooseMapper :: CartData -> IO Mapper
chooseMapper = chooseMapperH . cHeader

chooseMapperH :: Header -> IO Mapper
chooseMapperH header
    | (hMapper header) == 0 = (chooseMapper0 header)
    | otherwise = error ("Unsupported Mapper: " ++ show (hMapper header))


noMapper :: IO Mapper
noMapper = chooseMapper0' (0, 0)
