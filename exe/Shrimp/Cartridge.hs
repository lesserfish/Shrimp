module Shrimp.Cartridge where

import Control.Monad.State
import Data.Array
import Data.Bits
import qualified Data.ByteString as BS
import Data.Char
import Data.Word
import System.IO

data Mirroring = Horizontal | Vertical deriving (Eq, Show)
data ConsoleType = NES | NVS | Playchoice10 | Extended | Undefined deriving (Eq, Show)

getConsoleType :: Word8 -> ConsoleType
getConsoleType 0 = NES
getConsoleType 1 = NVS
getConsoleType 2 = Playchoice10
getConsoleType 3 = Extended
getConsoleType _ = Undefined

data Header = Header
    { hMagicNumbers :: [Word8]
    , hPrgSize :: Word8
    , hChrSize :: Word8
    , hChrRAM :: Bool
    , hMirroring :: Mirroring
    , hBattery :: Bool
    , hTrainer :: Bool
    , hFSVRam :: Bool
    , hlMapper :: Word8
    , huMapper :: Word8
    , hMapper :: Word8
    , hConsole :: ConsoleType
    , hNES2 :: Bool
    , hPRGRamSize :: Word8
    }
    deriving (Show)

data Cartridge = Cartridge
    { cHeader :: Header
    , cPRGData :: Array Word16 Word8
    , cCHRData :: Array Word16 Word8
    }
    deriving (Show)

emptyArray :: Int -> Array Word16 Word8
emptyArray size = listArray (0, fromIntegral size - 1) (replicate (fromIntegral size) 0)

emptyHeader :: Header
emptyHeader =
    Header
        { hMagicNumbers = [] -- Empty list of Word8
        , hPrgSize = 0 -- Default Word8 value
        , hChrSize = 0 -- Default Word8 value
        , hChrRAM = False -- Default Bool value
        , hMirroring = Horizontal -- Default Mirroring value
        , hBattery = False -- Default Bool value
        , hTrainer = False -- Default Bool value
        , hFSVRam = False -- Default Bool value
        , hlMapper = 0 -- Default Word8 value
        , huMapper = 0 -- Default Word8 value
        , hMapper = 0 -- Default Word8 value
        , hConsole = Undefined -- Default ConsoleType value
        , hNES2 = False -- Default Bool value
        , hPRGRamSize = 0 -- Default Word8 value
        }

emptyCartridge :: Cartridge
emptyCartridge = Cartridge{cHeader = emptyHeader, cPRGData = emptyArray 1, cCHRData = emptyArray 1}

readByteFromFile :: Handle -> IO Word8
readByteFromFile file = do
    byte' <- BS.hGet file 1
    let byte = BS.index byte' 0
    return byte

readBytesFromFile :: Handle -> Int -> IO [Word8]
readBytesFromFile file count = do
    bytes' <- BS.hGet file count
    let bytes = BS.unpack bytes' :: [Word8]
    return bytes

b0 x = testBit x 0

b1 x = testBit x 1

b2 x = testBit x 2

b3 x = testBit x 3

b4 x = testBit x 4

b5 x = testBit x 5

b6 x = testBit x 6

b7 x = testBit x 7

b8 x = testBit x 8

-- Header loading functions
loadHSize :: Handle -> StateT Header IO ()
loadHSize file = do
    header <- get
    _magicNumber <- liftIO $ readBytesFromFile file 4
    _prgSize <- liftIO $ readByteFromFile file
    _chrSize <- liftIO $ readByteFromFile file
    let _chrRAM = (_chrSize == 0)
    let header' =
            header
                { hMagicNumbers = _magicNumber
                , hPrgSize = _prgSize
                , hChrSize = _chrSize
                }
    put header'

loadHFlag6 :: Handle -> StateT Header IO ()
loadHFlag6 file = do
    header <- get
    _flag6 <- liftIO $ readByteFromFile file
    let _mirroring = if (b0 _flag6) then Vertical else Horizontal
    let _battery = b1 _flag6
    let _trainer = b2 _flag6
    let _fsvram = b3 _flag6
    let _lmapper = (_flag6 `shiftR` 4) .&. 0x0F

    let header' =
            header
                { hMirroring = _mirroring
                , hBattery = _battery
                , hTrainer = _trainer
                , hFSVRam = _fsvram
                , hlMapper = _lmapper
                }
    put header'

loadHFlag7 :: Handle -> StateT Header IO ()
loadHFlag7 file = do
    header <- get
    _flag7 <- liftIO $ readByteFromFile file
    let _console = getConsoleType (_flag7 .&. 0x03)
    let _useNES2f = b2 _flag7
    let _umapper = (_flag7 .&. 0xF0)
    let _mapper = (hlMapper header) + _umapper
    let header' =
            header
                { hConsole = _console
                , hNES2 = _useNES2f
                , huMapper = _umapper
                , hMapper = _mapper
                }
    put header'

-- TODO: Add INES2.0 Functionality
loadHFlag8 :: Handle -> StateT Header IO ()
loadHFlag8 file = do
    header <- get
    _flag8 <- liftIO $ readByteFromFile file
    let header' = header{hPRGRamSize = _flag8}
    put header

loadHFlag9 :: Handle -> StateT Header IO ()
loadHFlag9 file = do
    header <- get
    _flag9 <- liftIO $ readByteFromFile file
    -- TODO: Finishthis
    put header

loadHFlag10 :: Handle -> StateT Header IO ()
loadHFlag10 file = do
    header <- get
    _flag10 <- liftIO $ readByteFromFile file
    -- TODO: Finish this
    put header

loadHeader :: Handle -> StateT Header IO ()
loadHeader file = do
    loadHSize file
    loadHFlag6 file
    loadHFlag7 file
    loadHFlag8 file
    loadHFlag9 file
    loadHFlag10 file

-- Load Cartridge techniques
loadCHeader :: Handle -> StateT Cartridge IO ()
loadCHeader file = do
    cartridge <- get
    header <- liftIO $ execStateT (loadHeader file) emptyHeader
    let cartridge' = cartridge{cHeader = header}
    put cartridge'

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
loadCPCInstROM :: Handle -> StateT Cartridge IO ()
loadCPCInstROM file = do
    cartridge <- get
    -- TODO: Implement this
    put cartridge

loadCPCPROM :: Handle -> StateT Cartridge IO ()
loadCPCPROM file = do
    cartridge <- get
    -- TODO: Implement this
    put cartridge

loadCartridgeT :: Handle -> StateT Cartridge IO ()
loadCartridgeT file = do
    loadCHeader file
    loadCTrainer file
    loadCPRG file
    loadCCHR file
    loadCPCInstROM file
    loadCPCPROM file

loadCartridge :: FilePath -> IO Cartridge
loadCartridge filepath = do
    file <- openBinaryFile filepath ReadMode
    cartridge <- execStateT (loadCartridgeT file) emptyCartridge
    return cartridge
