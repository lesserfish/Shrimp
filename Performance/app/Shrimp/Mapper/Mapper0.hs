module Shrimp.Mapper.Mapper0 where

import Shrimp.Cartridge.Loader
import Data.Bits
import Data.Word
import Shrimp.Mapper.Type

data Mapper0 = Mapper0 {prgBanks0 :: Word8, chrBanks0 :: Word8}

mapper0_cr :: Mapper0 -> Word16 -> IO Word16
mapper0_cr m addr 
        | prgBanks0 m > 1 = return $ shifted_addr
        | otherwise       = return $ shifted_addr'
      where
        shifted_addr = addr .&. 0x7FFF
        shifted_addr' = addr .&. 0x3FFF

mapper0_cw :: Mapper0 -> Word16 -> IO Word16
mapper0_cw m addr
        | prgBanks0 m > 1 = return $ shifted_addr
        | otherwise = return $ shifted_addr'
      where
        shifted_addr = addr .&. 0x7FFF
        shifted_addr' = addr .&. 0x3FFF

mapper0_pr :: Mapper0 -> Word16 -> IO Word16
mapper0_pr _ addr = return $ addr

mapper0_pw :: Mapper0 -> Word16 -> IO Word16
mapper0_pw _ addr = return $ addr 

mapper0 :: Mapper0 -> Mapper
mapper0 m = Mapper (mapper0_cr m) (mapper0_cw m) (mapper0_pr m) (mapper0_pw m) (mapper0_cr m) (mapper0_pr m)

chooseMapper0 :: Header -> IO Mapper
chooseMapper0 header = return $ mapper0 m0 
    where
        m0 = Mapper0{prgBanks0 = (hPrgSize header), chrBanks0 = (hChrSize header)}

chooseMapper0' :: (Word8, Word8) -> IO Mapper
chooseMapper0' (prgBanks, chrBanks) = return $ mapper0 m0
    where
        m0 = Mapper0{prgBanks0 = prgBanks, chrBanks0 = chrBanks}
