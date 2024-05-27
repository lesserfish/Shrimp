module Shrimp.Cartridge (
    CartData(..),
    emptyCartridge,
    Cartridge(..),
    CartridgeIO,
    CartridgeST,
    fromCartData,
    fromCartDataIO,
    loadCartridgeIO,
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
    resetIO
) where

import Data.Word
import Shrimp.Cartridge.Loader (CartData(..), Mirroring(..), Header(..))
import qualified Shrimp.Cartridge.Loader as CL
import qualified Shrimp.Memory as M
import qualified Shrimp.Mapper.Mapper as Mapper
import qualified Shrimp.Mapper.Mapper0 as Mapper0

data Cartridge m = Cartridge
    { cartData :: CartData
    , prgData :: M.RAM m
    , chrData :: M.RAM m
    , mapper :: Mapper.Mapper
    }

type CartridgeIO = Cartridge M.RealWorld
type CartridgeST s = Cartridge s

emptyCartridge :: (M.PrimMonad m) => m ( Cartridge (M.PrimState m))
emptyCartridge = do
    noram <- M.noRAM
    let nomapper = Mapper.Mapper (Mapper0.Mapper0 0 0)
    let cart = Cartridge
                { cartData = CL.emptyCartData
                , prgData = noram
                , chrData = noram
                , mapper = nomapper}
    return cart

fromCartData :: (M.PrimMonad m) => CartData -> m ( Cartridge (M.PrimState m))
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

fromCartDataIO :: CartData -> IO CartridgeIO
fromCartDataIO cart = fromCartData cart

loadCartridgeIO :: FilePath -> IO CartridgeIO
loadCartridgeIO fp = do
    cartdata <- CL.loadCartData fp
    fromCartDataIO cartdata

-- CPU

cpuRead :: (M.PrimMonad m) => Cartridge (M.PrimState m) -> Word16 -> m (Cartridge (M.PrimState m), Word8)
cpuRead cart addr = do
    let (mapper', addr') = Mapper.cpuRMap (mapper cart) addr
    byte <- M.readByte (prgData cart) addr'
    let cart' = cart{mapper = mapper'}
    return (cart', byte)

cpuReadIO :: CartridgeIO -> Word16 -> IO (CartridgeIO, Word8)
cpuReadIO = cpuRead

cpuWrite :: (M.PrimMonad m) => Cartridge (M.PrimState m) -> Word16 -> Word8 -> m (Cartridge (M.PrimState m))
cpuWrite cart addr byte = do
    let (mapper', addr') = Mapper.cpuWMap (mapper cart) addr
    M.writeByte (prgData cart) addr' byte
    let cart' = cart{mapper = mapper'}
    return cart'

cpuWriteIO :: CartridgeIO -> Word16 -> Word8 -> IO CartridgeIO
cpuWriteIO = cpuWrite

-- PPU

ppuRead :: (M.PrimMonad m) => Cartridge (M.PrimState m) -> Word16 -> m (Cartridge (M.PrimState m), Word8)
ppuRead cart addr = do
    let (mapper', addr') = Mapper.ppuRMap (mapper cart) addr
    byte <- M.readByte (chrData cart) addr'
    let cart' = cart{mapper = mapper'}
    return (cart', byte)

ppuReadIO :: CartridgeIO -> Word16 -> IO (CartridgeIO, Word8)
ppuReadIO = ppuRead

ppuWrite :: (M.PrimMonad m) => Cartridge (M.PrimState m) -> Word16 -> Word8 -> m (Cartridge (M.PrimState m))
ppuWrite cart addr byte = do
    let (mapper', addr') = Mapper.ppuWMap (mapper cart) addr
    M.writeByte (chrData cart) addr' byte
    let cart' = cart{mapper = mapper'}
    return cart'

ppuWriteIO :: CartridgeIO -> Word16 -> Word8 -> IO CartridgeIO
ppuWriteIO = ppuWrite


reset :: (M.PrimMonad m) => Cartridge (M.PrimState m) -> m (Cartridge (M.PrimState m))
reset cart = undefined -- TODO

resetIO :: CartridgeIO -> IO CartridgeIO
resetIO = reset
