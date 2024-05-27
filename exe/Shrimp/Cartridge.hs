module Shrimp.Cartridge (
    CartData(..),
    emptyCartridge,
    Cartridge(..),
    fromCartData,
    loadCartridge,
    Mirroring(..),
    Header(..),
    cpuRead,
    cpuWrite,
    ppuRead,
    ppuWrite,
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
    , prgData :: M.RAM
    , chrData :: M.RAM
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

fromCartData ::  CartData -> IO Cartridge
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

loadCartridge :: FilePath -> IO Cartridge
loadCartridge fp = do
    cartdata <- CL.loadCartData fp
    fromCartData cartdata

-- CPU

cpuRead ::  Cartridge  -> Word16 -> IO (Cartridge, Word8)
cpuRead cart addr = do
    let (mapper', addr') = Mapper.cpuRMap (mapper cart) addr
    byte <- M.readByte (prgData cart) addr'
    let cart' = cart{mapper = mapper'}
    return (cart', byte)

cpuWrite ::  Cartridge  -> Word16 -> Word8 -> IO Cartridge
cpuWrite cart addr byte = do
    let (mapper', addr') = Mapper.cpuWMap (mapper cart) addr
    M.writeByte (prgData cart) addr' byte
    let cart' = cart{mapper = mapper'}
    return cart'

-- PPU

ppuRead ::  Cartridge  -> Word16 -> IO (Cartridge, Word8)
ppuRead cart addr = do
    let (mapper', addr') = Mapper.ppuRMap (mapper cart) addr
    byte <- M.readByte (chrData cart) addr'
    let cart' = cart{mapper = mapper'}
    return (cart', byte)

ppuWrite ::  Cartridge  -> Word16 -> Word8 -> IO Cartridge
ppuWrite cart addr byte = do
    let (mapper', addr') = Mapper.ppuWMap (mapper cart) addr
    M.writeByte (chrData cart) addr' byte
    let cart' = cart{mapper = mapper'}
    return cart'

reset ::  Cartridge  -> IO Cartridge
reset cart = undefined -- TODO

