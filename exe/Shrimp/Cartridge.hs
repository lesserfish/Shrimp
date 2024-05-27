module Shrimp.Cartridge (
    CartData(..),
    emptyCartridge,
    Cartridge(..),
    Cartridge,
    fromCartData,
    fromCartDataIO,
    loadCartridge,
    Mirroring(..),
    Header(..),
    cpuRead,
    cpuReadIO,
    cpuWrite,
    cpuWriteIO,
    ppuRead,
    ppuReadIO,
    ppuWrite,
    ppuWriteIO,
    reset,
) where

import Data.Word
import Shrimp.Cartridge.Loader (CartData(..), Mirroring(..), Header(..))
import qualified Shrimp.Cartridge.Loader as CL
import qualified Shrimp.Memory as M
import qualified Shrimp.Mapper.Mapper as Mapper
import qualified Shrimp.Mapper.Mapper0 as Mapper0

data Cartridge = Cartridge
    { cartData :: CartData
    , prgData :: M.IORAM
    , chrData :: M.IORAM
    , mapper :: Mapper.Mapper
    }

emptyCartridge ::  IO Cartridge
emptyCartridge = do
    noram <- M.noRAM
    let nomapper = Mapper.Mapper (Mapper0.Mapper0 0 0)
    let cart = Cartridge
                { cartData = CL.emptyCartData
                , prgData = noram
                , chrData = noram
                , mapper = nomapper}
    return cart

fromCartData ::  CartData -> IO ( Cartridge )
fromCartData cartdata = do
    let prgsize = 0x4000 * (fromIntegral . hPrgSize . cHeader $ cartdata) :: Int
    let chrsize = 0x2000 * (fromIntegral . hChrSize . cHeader $ cartdata) :: Int
    prgram <- M.new prgsize 0
    chrram <- M.new chrsize 0
    M.loadList prgram 0 (cPRGData cartdata)
    M.loadList chrram 0 (cCHRData cartdata)
    let cmapper = Mapper.chooseMapper cartdata
    let cart = Cartridge
                { cartData = CL.emptyCartData
                , prgData = prgram
                , chrData = chrram
                , mapper = cmapper}
    return cart

fromCartDataIO :: CartData -> IO Cartridge
fromCartDataIO cart = fromCartData cart

loadCartridge :: FilePath -> IO Cartridge
loadCartridge fp = do
    cartdata <- CL.loadCartData fp
    fromCartDataIO cartdata

-- CPU

cpuRead ::  Cartridge  -> Word16 -> IO (Cartridge, Word8)
cpuRead cart addr = do
    let (mapper', addr') = Mapper.cpuRMap (mapper cart) addr
    byte <- M.readByte (prgData cart) addr'
    let cart' = cart{mapper = mapper'}
    return (cart', byte)

cpuReadIO :: Cartridge -> Word16 -> IO (Cartridge, Word8)
cpuReadIO = cpuRead

cpuWrite ::  Cartridge  -> Word16 -> Word8 -> IO (Cartridge)
cpuWrite cart addr byte = do
    let (mapper', addr') = Mapper.cpuWMap (mapper cart) addr
    M.writeByte (prgData cart) addr' byte
    let cart' = cart{mapper = mapper'}
    return cart'

cpuWriteIO :: Cartridge -> Word16 -> Word8 -> IO Cartridge
cpuWriteIO = cpuWrite

-- PPU

ppuRead ::  Cartridge  -> Word16 -> IO (Cartridge, Word8)
ppuRead cart addr = do
    let (mapper', addr') = Mapper.ppuRMap (mapper cart) addr
    byte <- M.readByte (chrData cart) addr'
    let cart' = cart{mapper = mapper'}
    return (cart', byte)

ppuReadIO :: Cartridge -> Word16 -> IO (Cartridge, Word8)
ppuReadIO = ppuRead

ppuWrite ::  Cartridge  -> Word16 -> Word8 -> IO (Cartridge)
ppuWrite cart addr byte = do
    let (mapper', addr') = Mapper.ppuWMap (mapper cart) addr
    M.writeByte (chrData cart) addr' byte
    let cart' = cart{mapper = mapper'}
    return cart'

ppuWriteIO :: Cartridge -> Word16 -> Word8 -> IO Cartridge
ppuWriteIO = ppuWrite


reset ::  Cartridge  -> IO (Cartridge)
reset cart = undefined -- TODO

