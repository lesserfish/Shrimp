module Shrimp.Cartridge (
    module Shrimp.Cartridge.Loader,
    Cartridge(..),
    fromCartData,
    loadCartridge,
    cpuWrite,
    cpuRead,
    cpuPeek,
    ppuWrite,
    ppuRead,
    ppuPeek,
    reset
    ) where

import Data.Word
import Shrimp.Cartridge.Loader
import qualified Shrimp.Memory as Memory
import qualified Shrimp.Mapper as Mapper

data Cartridge = Cartridge
    { cartData :: CartData
    , prgData :: Memory.RAM
    , chrData :: Memory.RAM
    , mapper :: Mapper.Mapper
    }

emptyCartridge ::  IO Cartridge
emptyCartridge = do
    noram <- Memory.noRAM
    nomapper <- Mapper.noMapper
    let cart = Cartridge
                { cartData = emptyCartData
                , prgData = noram
                , chrData = noram
                , mapper = nomapper}
    return cart

fromCartData ::  CartData -> IO Cartridge
fromCartData cartdata = do
    let prgsize = 0x4000 * (fromIntegral . hPrgSize . cHeader $ cartdata) :: Int
    let chrsize = 0x2000 * (fromIntegral . hChrSize . cHeader $ cartdata) :: Int
    prgram <- Memory.new prgsize 0
    chrram <- Memory.new chrsize 0
    Memory.loadList prgram 0 (cPRGData cartdata)
    Memory.loadList chrram 0 (cCHRData cartdata)
    cmapper <- Mapper.chooseMapper cartdata
    let cart = Cartridge
                { cartData = cartdata
                , prgData = prgram
                , chrData = chrram
                , mapper = cmapper}
    return cart

loadCartridge :: FilePath -> IO Cartridge
loadCartridge fp = do
    cartdata <- loadCartData fp
    fromCartData cartdata

-- CPU

cpuRead ::  Cartridge  -> Word16 -> IO Word8
cpuRead cart addr = do
    addr' <- Mapper.cpuRMap (mapper cart) addr
    Memory.readByte (prgData cart) addr'

cpuWrite ::  Cartridge  -> Word16 -> Word8 -> IO ()
cpuWrite cart addr byte = do
    addr' <- Mapper.cpuWMap (mapper cart) addr
    Memory.writeByte (prgData cart) addr' byte

cpuPeek :: Cartridge -> Word16 -> IO Word8
cpuPeek cart addr = do
    addr' <- Mapper.cpuPMap (mapper cart) addr
    Memory.readByte (prgData cart) addr'

-- PPU

ppuRead ::  Cartridge  -> Word16 -> IO Word8
ppuRead cart addr = do
    addr' <- Mapper.ppuRMap (mapper cart) addr
    Memory.readByte (chrData cart) addr'

ppuWrite ::  Cartridge  -> Word16 -> Word8 -> IO ()
ppuWrite cart addr byte = do
    addr' <- Mapper.ppuWMap (mapper cart) addr
    Memory.writeByte (chrData cart) addr' byte

ppuPeek :: Cartridge -> Word16 -> IO Word8
ppuPeek cart addr = do
    addr' <- Mapper.ppuPMap (mapper cart) addr
    Memory.readByte (chrData cart) addr'

reset ::  Cartridge  -> IO ()
reset cart = return () -- TODO

