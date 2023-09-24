module Shrimp.Cartridge (
    Cartridge (..),
    emptyCartridge,
    loadCartridge,
) where

import Control.Monad.State
import Data.Array
import Data.Bits
import qualified Data.ByteString as BS
import Data.Char
import Data.Word
import Shrimp.INES
import Shrimp.Mapper.Mapper
import Shrimp.Mapper.Mapper0
import System.IO

data Cartridge = Cartridge
    { cHeader :: Header
    , cPRGData :: Array Word16 Word8
    , cCHRData :: Array Word16 Word8
    , cMapper :: Mapper
    }
    deriving (Show)

-- A default empty Array of size N
emptyArray :: Int -> Array Word16 Word8
emptyArray size = listArray (0, fromIntegral size - 1) (replicate (fromIntegral size) 0)

-- A default empty cartridge
emptyCartridge :: Cartridge
emptyCartridge = Cartridge{cHeader = emptyHeader, cPRGData = emptyArray 1, cCHRData = emptyArray 1, cMapper = Mapper (Mapper0 0 0)}

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
    let prgArray = listArray (0, fromIntegral size - 1) prgData :: Array Word16 Word8
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
    let charArray = listArray (0, fromIntegral size - 1) charData :: Array Word16 Word8
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
