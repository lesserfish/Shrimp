module Cartridge (
    Cartridge(..),
    CartData(..),
    Mirroring(..),
    Header(..),
    fromCartData,
    loadCartridge,
    cpuWrite,
    cpuRead,
    ppuWrite,
    ppuRead,
    reset
    ) where

import Data.Word
import Cartridge.Loader (CartData(..), Mirroring(..), Header(..))
import qualified Cartridge.Loader as CL
import qualified Memory as M
import qualified Mapper as Mapper
import qualified Mapper.Mapper0 as Mapper0

data Cartridge = Cartridge
    { cartData :: CartData
    , prgData :: M.RAM
    , chrData :: M.RAM
    , mapper :: Mapper.Mapper
    }

emptyCartridge ::  IO Cartridge
emptyCartridge = do
    noram <- M.noRAM
    nomapper <- Mapper0.chooseMapper0' (0, 0)
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
    cmapper <- Mapper.chooseMapper cartdata
    let cart = Cartridge
                { cartData = cartdata
                , prgData = prgram
                , chrData = chrram
                , mapper = cmapper}
    return cart

loadCartridge :: FilePath -> IO Cartridge
loadCartridge fp = do
    cartdata <- CL.loadCartData fp
    fromCartData cartdata

-- CPU

cpuRead ::  Cartridge  -> Word16 -> IO Word8
cpuRead cart addr = do
    addr' <- Mapper.cpuRMap (mapper cart) addr
    byte <- M.readByte (prgData cart) addr'
    return byte

cpuWrite ::  Cartridge  -> Word16 -> Word8 -> IO ()
cpuWrite cart addr byte = do
    addr' <- Mapper.cpuWMap (mapper cart) addr
    M.writeByte (prgData cart) addr' byte

-- PPU

ppuRead ::  Cartridge  -> Word16 -> IO Word8
ppuRead cart addr = do
    addr' <- Mapper.ppuRMap (mapper cart) addr
    byte <- M.readByte (chrData cart) addr'
    return byte

ppuWrite ::  Cartridge  -> Word16 -> Word8 -> IO ()
ppuWrite cart addr byte = do
    addr' <- Mapper.ppuWMap (mapper cart) addr
    M.writeByte (chrData cart) addr' byte

reset ::  Cartridge  -> IO ()
reset cart = return () -- TODO

