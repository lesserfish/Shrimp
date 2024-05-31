module Shrimp.Cartridge.Loader (
    Mirroring(..),
    ConsoleType(..),
    Header(..),
    CartData(..),
    emptyCartData,
    emptyHeader,
    loadCartData
) where

import Control.Monad.State
import Data.Bits
import qualified Data.ByteString as BS
import Data.Word
import System.IO

data Mirroring = Horizontal | Vertical deriving (Eq, Show)
data ConsoleType = NES | NVS | Playchoice10 | Extended | Undefined deriving (Eq, Show)

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

data CartData = CartData
    { cHeader :: Header
    , cPRGData :: [Word8]
    , cCHRData :: [Word8]
    }
    deriving (Show)

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

emptyCartData :: CartData
emptyCartData = 
    CartData
        { cHeader = emptyHeader
        , cPRGData = []
        , cCHRData = []
        }

getConsoleType :: Word8 -> ConsoleType
getConsoleType 0 = NES
getConsoleType 1 = NVS
getConsoleType 2 = Playchoice10
getConsoleType 3 = Extended
getConsoleType _ = Undefined

-- Helper Functions

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

getH :: (Monad m) => StateT CartData m Header
getH = cHeader <$> get

putH :: (Monad m) => Header -> StateT CartData m ()
putH h = modify (\cart -> cart {cHeader = h})


-- Header extraction


-- Load the Magic number, and PRG and CHR sizing information
loadHSize :: Handle -> StateT CartData IO ()
loadHSize file = do
    header <- getH
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
    putH header'

-- Load Flag 6
loadHFlag6 :: Handle -> StateT CartData IO ()
loadHFlag6 file = do
    header <- getH
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
    putH header'

-- Load Flag 7
loadHFlag7 :: Handle -> StateT CartData IO ()
loadHFlag7 file = do
    header <- getH
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
    putH header'

-- Load Flag 8
loadHFlag8 :: Handle -> StateT CartData IO ()
loadHFlag8 file = do
    header <- getH
    _flag8 <- liftIO $ readByteFromFile file
    let header' = header{hPRGRamSize = _flag8}
    putH header

-- Load Flag 9
loadHFlag9 :: Handle -> StateT CartData IO ()
loadHFlag9 file = do
    header <- getH
    _flag9 <- liftIO $ readByteFromFile file
    -- TODO: Finishthis
    putH header

-- Load Flag 10
loadHFlag10 :: Handle -> StateT CartData IO ()
loadHFlag10 file = do
    header <- getH
    _flag10 <- liftIO $ readByteFromFile file
    -- TODO: Finish this
    putH header

loadPadding :: Handle -> StateT CartData IO ()
loadPadding file = do
    padding <- liftIO $ readBytesFromFile file 5
    return ()

-- Loads the Header
-- TODO: Implement INES2.0 compatibility
loadHeader :: Handle -> StateT CartData IO ()
loadHeader file = do
    loadHSize file
    loadHFlag6 file
    loadHFlag7 file
    loadHFlag8 file
    loadHFlag9 file
    loadHFlag10 file
    loadPadding file

-- Cartridge Data


-- Loads the PRG Data
loadCPRG :: Handle -> StateT CartData IO ()
loadCPRG file = do
    cartridge <- get
    let header = cHeader cartridge
    let prgBanks = hPrgSize header
    let size = (fromIntegral prgBanks) * 16 * 1024 :: Int
    prgData <- liftIO $ readBytesFromFile file size 
    let cartridge' = cartridge{cPRGData = prgData}
    put cartridge'

--
-- Loads the CHR Data
loadCCHR :: Handle -> StateT CartData IO ()
loadCCHR file = do
    cartridge <- get
    let header = cHeader cartridge
    let chrBanks = hChrSize header
    let size = (fromIntegral chrBanks) * 8 * 1024 :: Int
    charData <- liftIO $ readBytesFromFile file size
    let cartridge' = cartridge{cCHRData = charData}
    put cartridge'

loadCTrainer :: Handle -> StateT CartData IO ()
loadCTrainer file = do
    cartridge <- get
    -- TODO: Implement this
    put cartridge

-- Loads the PlayChoice INST-ROM, if present
-- TODO: Implement this
loadCPCInstROM :: Handle -> StateT CartData IO ()
loadCPCInstROM file = do
    cartridge <- get
    put cartridge

-- Loads the PlayChoice PROM, if present
-- TODO: Implement this
loadCPCPROM :: Handle -> StateT CartData IO ()
loadCPCPROM file = do
    cartridge <- get
    put cartridge

loadCartDataT :: Handle -> StateT CartData IO ()
loadCartDataT file = do
    loadHeader file -- Loads the Header
    loadCTrainer file -- Loads the Trainer section, if present
    loadCPRG file -- Loads the PRG Data
    loadCCHR file -- Loads the CHR Data
    loadCPCInstROM file -- Loads the PlayChoice INST-ROM, if present
    loadCPCPROM file -- Loads the PlayChoice PROM, if present

loadCartData :: FilePath -> IO CartData
loadCartData filepath = do
    file <- openBinaryFile filepath ReadMode
    cartridge <- execStateT (loadCartDataT file) emptyCartData
    return cartridge
