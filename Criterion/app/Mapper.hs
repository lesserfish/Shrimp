{-# LANGUAGE GADTs #-}

module Mapper (chooseMapper, Mapper(..)) where

import Mapper.Type
import Mapper.Mapper0
import Cartridge.Loader


chooseMapper :: CartData -> Mapper
chooseMapper = chooseMapperH . cHeader

chooseMapperH :: Header -> Mapper
chooseMapperH header
    | (hMapper header) == 0 = (chooseMapper0 header)
    | otherwise = error ("Unsupported Mapper: " ++ show (hMapper header))
