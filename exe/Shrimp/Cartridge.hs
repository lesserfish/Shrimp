module Shrimp.Cartridge (
    Cartridge (..),
    emptyCartridge,
    loadCartridge,
    cpuWrite,
    cpuRead,
    ppuWrite,
    ppuRead,
    reset,
) where

import Control.Monad.State
import Data.Bits
import qualified Data.ByteString as BS
import Data.Char
import Data.Word
import Shrimp.INES
import qualified Shrimp.Mapper.AbstractMapper as AMapper
import Shrimp.Mapper.Mapper
import Shrimp.Mapper.Mapper0
import qualified Shrimp.Memory as Memory
import System.IO

data Cartridge = Cartridge
    { cHeader :: Header
    , cPRGData :: Memory.RAM
    , cCHRData :: Memory.RAM
    , cMapper :: Mapper
    }
    deriving (Show)

-- A default empty cartridge
emptyCartridge :: Cartridge
emptyCartridge = Cartridge{cHeader = emptyHeader, cPRGData = Memory.noRAM, cCHRData = Memory.noRAM, cMapper = Mapper (Mapper0 0 0)}

-- Reads a singly byte from file and return a Word8
readByteFromFile :: Handle -> IO Word8
readByteFromFile file = do
    byte' <- BS.hGet file 1
    let byte = BS.index byte' 0
    return byte

-- Reads N bytes from file and returns a [Word8]
readBytesFromFile :: Handle -> Int -> IO [Word8]
readBytesFromFile file count = do
    bytes' <- BS.hGet file count
    let bytes = BS.unpack bytes' :: [Word8]
    return bytes

-- Loads the Header into the Cartridge
loadCHeader :: Handle -> StateT Cartridge IO ()
loadCHeader file = do
    cartridge <- get
    header <- liftIO $ execStateT (loadHeader file) emptyHeader
    let mapper = chooseMapper header
    let cartridge' = cartridge{cHeader = header, cMapper = mapper}
    put cartridge'

-- Loads the PRG Data
loadCPRG :: Handle -> StateT Cartridge IO ()
loadCPRG file = do
    cartridge <- get
    let header = cHeader cartridge
    let prgBanks = hPrgSize header
    let size = (fromIntegral prgBanks) * 16 * 1024 :: Int
    liftIO . putStrLn $ "Reading " ++ (show size) ++ " bytes of PRG data"
    prgData <- liftIO $ readBytesFromFile file size
    let prgArray = Memory.fromList prgData
    let cartridge' = cartridge{cPRGData = prgArray}
    put cartridge'

--
-- Loads the CHR Data
loadCCHR :: Handle -> StateT Cartridge IO ()
loadCCHR file = do
    cartridge <- get
    let header = cHeader cartridge
    let chrBanks = hChrSize header
    let size = (fromIntegral chrBanks) * 8 * 1024 :: Int
    liftIO . putStrLn $ "Reading " ++ (show size) ++ " bytes of CHR data"
    charData <- liftIO $ readBytesFromFile file size
    let charArray = Memory.fromList charData
    let cartridge' = cartridge{cCHRData = charArray}
    put cartridge'

loadCTrainer :: Handle -> StateT Cartridge IO ()
loadCTrainer file = do
    cartridge <- get
    -- TODO: Implement this
    put cartridge

-- Loads the PlayChoice INST-ROM, if present
-- TODO: Implement this
loadCPCInstROM :: Handle -> StateT Cartridge IO ()
loadCPCInstROM file = do
    cartridge <- get
    put cartridge

-- Loads the PlayChoice PROM, if present
-- TODO: Implement this
loadCPCPROM :: Handle -> StateT Cartridge IO ()
loadCPCPROM file = do
    cartridge <- get
    put cartridge

loadCartridgeT :: Handle -> StateT Cartridge IO ()
loadCartridgeT file = do
    loadCHeader file -- Loads the Header
    loadCTrainer file -- Loads the Trainer section, if present
    loadCPRG file -- Loads the PRG Data
    loadCCHR file -- Loads the CHR Data
    loadCPCInstROM file -- Loads the PlayChoice INST-ROM, if present
    loadCPCPROM file -- Loads the PlayChoice PROM, if present

loadCartridge :: FilePath -> IO Cartridge
loadCartridge filepath = do
    file <- openBinaryFile filepath ReadMode
    cartridge <- execStateT (loadCartridgeT file) emptyCartridge
    return cartridge

cpuRead :: Cartridge -> Word16 -> (Cartridge, Word8)
cpuRead cart addr = (cartridge', byte)
  where
    (mapper', addr') = AMapper.cpuRMap (cMapper cart) addr
    byte = Memory.readByte (cPRGData cart) addr'
    cartridge' = cart{cMapper = mapper'}

-- TODO: Is Write allowed? Is PRG ROM or RAM?
cpuWrite :: Cartridge -> Word16 -> Word8 -> Cartridge
cpuWrite cart addr byte = cartridge'
  where
    (mapper', addr') = AMapper.cpuWMap (cMapper cart) addr
    prg' = Memory.writeByte (cPRGData cart) addr' byte
    cartridge' = cart{cMapper = mapper', cPRGData = prg'}

ppuRead :: Cartridge -> Word16 -> (Cartridge, Word8)
ppuRead cart addr = (cartridge', byte)
  where
    (mapper', addr') = AMapper.ppuRMap (cMapper cart) addr
    byte = Memory.readByte (cCHRData cart) addr'
    cartridge' = cart{cMapper = mapper'}

-- TODO: Is Write allowed? Is CHR ROM or RAM?
ppuWrite :: Cartridge -> Word16 -> Word8 -> Cartridge
ppuWrite cart addr byte = cartridge'
  where
    (mapper', addr') = AMapper.ppuWMap (cMapper cart) addr
    chr' = Memory.writeByte (cCHRData cart) addr' byte
    cartridge' = cart{cMapper = mapper', cCHRData = chr'}

reset :: Cartridge -> Cartridge
reset cart = cart'
  where
    mapper' = AMapper.reset (cMapper cart)
    cart' = cart{cMapper = mapper'}
