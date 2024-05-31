module Shrimp.R2C02 where

import Control.Monad (when)
import Control.Monad.State
import Data.Bits
import Data.Word
import Text.Printf
import Shrimp.Utils


-- Registers

data CTRLFLAG
    = C_NAMETABLE_X
    | C_NAMETABLE_Y
    | C_INCREMENT_MODE
    | C_PATTERN_SPRITE
    | C_PATTERN_BACKGROUND
    | C_SPRITE_SIZE
    | C_SLAVE_MDOE
    | C_ENABLE_NMI

data MASKFLAG
    = M_GRAYSCALE
    | M_RENDER_BACKGROUND_LEFT
    | M_RENDER_SPRITES_LEFT
    | M_RENDER_BACKGROUND
    | M_RENDER_SPRITES
    | M_ENHANCE_RED
    | M_ENHANCE_GREEN
    | M_ENHANCE_BLUE

data STATUSFLAG
    = S_SPRITE_OVERFLOW
    | S_SPRITE_ZERO_HIT
    | S_VERTICAL_BLANK

data LOOPYFLAG
    = L_COARSE_X
    | L_COARSE_Y
    | L_NAMETABLE_X
    | L_NAMETABLE_Y
    | L_FINE_Y

data Context = Context
    { ppuNMI :: Bool
    , complete :: Bool
    , ppuScanline :: Int
    , ppuCycle :: Int
    , shifterPatternLo :: Word16
    , shifterPatternHi :: Word16
    , shifterAttribLo :: Word16
    , shifterAttribHi :: Word16
    , nextTileLsb :: Word16
    , nextTileMsb :: Word16
    , nextTileID :: Word16
    , nextTileAttrib :: Word16
    , bgPixel :: Word8
    , bgPalette :: Word8
    }

data Registers = Registers
    { ppuctrl       :: Word8
    , ppumask       :: Word8
    , ppustatus     :: Word8
    , fineX         :: Word8
    , ppuDataBuffer :: Word8
    , vram          :: Word16
    , tram          :: Word16
    , writeToggle   :: Bool
    }


data Interface = Interface
    { iReadByte :: Word16 -> IO Word8
    , iWriteByte :: Word16 -> Word8 -> IO ()
    , iSetPixel :: (Word16, Word16) -> Word8 -> IO ()
    , iTriggerNMI :: IO ()
    , iPeekByte :: Word16 -> IO Word8
    }

data R2C02 = R2C02
    { registers :: Registers
    , context :: Context
    , interface :: Interface
    }

-- Creation

new :: Interface -> R2C02
new interface = R2C02 reg ctx interface where
    reg = Registers 0 0 0 0 0 0 0 False
    ctx = Context False False 0 0 0 0 0 0 0 0 0 0 0 0

reset :: StateT R2C02 IO ()
reset = do
    ppu <- get
    let ppu' = new (interface ppu)
    put ppu'

-- Registers Setters / Getters

mapControl :: (Word8 -> Word8) -> StateT R2C02 IO ()
mapControl f = modify (\ppu -> ppu{registers = (registers ppu){ppuctrl = f . ppuctrl . registers $ ppu}})


mapMask :: (Word8 -> Word8) -> StateT R2C02 IO ()
mapMask f = modify (\ppu -> ppu {registers = (registers ppu){ppumask = f . ppumask . registers $ ppu}})


mapStatus :: (Word8 -> Word8) -> StateT R2C02 IO ()
mapStatus f = modify (\ppu -> ppu {registers = (registers ppu){ppustatus = f . ppustatus . registers $ ppu}})


mapFineX :: (Word8 -> Word8) -> StateT R2C02 IO ()
mapFineX f = modify (\ppu -> ppu {registers = (registers ppu){fineX = f . fineX . registers $ ppu}})


mapDataBuffer :: (Word8 -> Word8) -> StateT R2C02 IO ()
mapDataBuffer f = modify (\ppu -> ppu {registers = (registers ppu){ppuDataBuffer = f . ppuDataBuffer . registers $ ppu}})


mapVRAM :: (Word16 -> Word16) -> StateT R2C02 IO ()
mapVRAM f = modify (\ppu -> ppu {registers = (registers ppu){vram = f . vram . registers $ ppu}})


mapTRAM :: (Word16 -> Word16) -> StateT R2C02 IO ()
mapTRAM f = modify (\ppu -> ppu {registers = (registers ppu){tram = f . tram . registers $ ppu}})


mapWriteToggle :: (Bool -> Bool) -> StateT R2C02 IO ()
mapWriteToggle f = modify (\ppu -> ppu {registers = (registers ppu){writeToggle = (f . writeToggle . registers $ ppu)}})



setControl :: Word8 -> StateT R2C02 IO ()
setControl value = mapControl (\_ -> value)


setMask :: Word8 -> StateT R2C02 IO ()
setMask value = mapMask (\_ -> value)


setStatus :: Word8 -> StateT R2C02 IO ()
setStatus value = mapStatus (\_ -> value)


setFineX :: Word8 -> StateT R2C02 IO ()
setFineX value = mapFineX (\_ -> value)


setDataBuffer :: Word8 -> StateT R2C02 IO ()
setDataBuffer value = mapDataBuffer (\_ -> value)


setVRAM :: Word16 -> StateT R2C02 IO ()
setVRAM value = mapVRAM (\_ -> value)


setTRAM :: Word16 -> StateT R2C02 IO ()
setTRAM value = mapTRAM (\_ -> value)


setWriteToggle :: Bool -> StateT R2C02 IO ()
setWriteToggle value = mapWriteToggle (\_ -> value)



getControl :: StateT R2C02 IO Word8
getControl = (ppuctrl . registers) <$> get


getMask :: StateT R2C02 IO Word8
getMask = (ppumask . registers) <$> get


getStatus :: StateT R2C02 IO Word8
getStatus = (ppustatus . registers) <$> get


getFineX :: StateT R2C02 IO Word8
getFineX = (fineX . registers) <$> get


getDataBuffer :: StateT R2C02 IO Word8
getDataBuffer = (ppuDataBuffer . registers) <$> get


getVRAM :: StateT R2C02 IO Word16
getVRAM = (vram . registers) <$> get


getTRAM :: StateT R2C02 IO Word16
getTRAM = (tram . registers) <$> get


getWriteToggle :: StateT R2C02 IO Bool
getWriteToggle = (writeToggle . registers) <$> get



-- Flag Setters / Getters


getCTRLFlag :: CTRLFLAG -> StateT R2C02 IO Bool
getCTRLFlag C_NAMETABLE_X           = b0 <$> getControl
getCTRLFlag C_NAMETABLE_Y           = b1 <$> getControl
getCTRLFlag C_INCREMENT_MODE        = b2 <$> getControl
getCTRLFlag C_PATTERN_SPRITE        = b3 <$> getControl
getCTRLFlag C_PATTERN_BACKGROUND    = b4 <$> getControl
getCTRLFlag C_SPRITE_SIZE           = b5 <$> getControl
getCTRLFlag C_SLAVE_MDOE            = b6 <$> getControl
getCTRLFlag C_ENABLE_NMI            = b7 <$> getControl


setCTRLFlag :: CTRLFLAG -> Bool -> StateT R2C02 IO ()
setCTRLFlag C_NAMETABLE_X v         = mapControl (\ctrl -> if v then setBit ctrl 0 else clearBit ctrl 0)
setCTRLFlag C_NAMETABLE_Y v         = mapControl (\ctrl -> if v then setBit ctrl 1 else clearBit ctrl 1)
setCTRLFlag C_INCREMENT_MODE v      = mapControl (\ctrl -> if v then setBit ctrl 2 else clearBit ctrl 2)
setCTRLFlag C_PATTERN_SPRITE v      = mapControl (\ctrl -> if v then setBit ctrl 3 else clearBit ctrl 3)
setCTRLFlag C_PATTERN_BACKGROUND v  = mapControl (\ctrl -> if v then setBit ctrl 4 else clearBit ctrl 4)
setCTRLFlag C_SPRITE_SIZE v         = mapControl (\ctrl -> if v then setBit ctrl 5 else clearBit ctrl 5)
setCTRLFlag C_SLAVE_MDOE v          = mapControl (\ctrl -> if v then setBit ctrl 6 else clearBit ctrl 6)
setCTRLFlag C_ENABLE_NMI v          = mapControl (\ctrl -> if v then setBit ctrl 7 else clearBit ctrl 7)


getMASKFlag :: MASKFLAG -> StateT R2C02 IO Bool
getMASKFlag M_GRAYSCALE                 = b0 <$> getMask
getMASKFlag M_RENDER_BACKGROUND_LEFT    = b1 <$> getMask
getMASKFlag M_RENDER_SPRITES_LEFT       = b2 <$> getMask
getMASKFlag M_RENDER_BACKGROUND         = b3 <$> getMask
getMASKFlag M_RENDER_SPRITES            = b4 <$> getMask
getMASKFlag M_ENHANCE_RED               = b5 <$> getMask
getMASKFlag M_ENHANCE_GREEN             = b6 <$> getMask
getMASKFlag M_ENHANCE_BLUE              = b7 <$> getMask


setMASKFlag :: MASKFLAG -> Bool -> StateT R2C02 IO ()
setMASKFlag M_GRAYSCALE v               = mapMask (\ctrl -> if v then setBit ctrl 0 else clearBit ctrl 0)
setMASKFlag M_RENDER_BACKGROUND_LEFT v  = mapMask (\ctrl -> if v then setBit ctrl 1 else clearBit ctrl 1)
setMASKFlag M_RENDER_SPRITES_LEFT v     = mapMask (\ctrl -> if v then setBit ctrl 2 else clearBit ctrl 2)
setMASKFlag M_RENDER_BACKGROUND v       = mapMask (\ctrl -> if v then setBit ctrl 3 else clearBit ctrl 3)
setMASKFlag M_RENDER_SPRITES v          = mapMask (\ctrl -> if v then setBit ctrl 4 else clearBit ctrl 4)
setMASKFlag M_ENHANCE_RED v             = mapMask (\ctrl -> if v then setBit ctrl 5 else clearBit ctrl 5)
setMASKFlag M_ENHANCE_GREEN v           = mapMask (\ctrl -> if v then setBit ctrl 6 else clearBit ctrl 6)
setMASKFlag M_ENHANCE_BLUE v            = mapMask (\ctrl -> if v then setBit ctrl 7 else clearBit ctrl 7)


getSTATUSFlag :: STATUSFLAG -> StateT R2C02 IO Bool
getSTATUSFlag S_SPRITE_OVERFLOW     = b5 <$> getStatus
getSTATUSFlag S_SPRITE_ZERO_HIT     = b6 <$> getStatus
getSTATUSFlag S_VERTICAL_BLANK      = b7 <$> getStatus


setSTATUSFlag :: STATUSFLAG -> Bool -> StateT R2C02 IO ()
setSTATUSFlag S_SPRITE_OVERFLOW v       = mapStatus (\ctrl -> if v then setBit ctrl 5 else clearBit ctrl 5)
setSTATUSFlag S_SPRITE_ZERO_HIT v       = mapStatus (\ctrl -> if v then setBit ctrl 6 else clearBit ctrl 6)
setSTATUSFlag S_VERTICAL_BLANK v        = mapStatus (\ctrl -> if v then setBit ctrl 7 else clearBit ctrl 7)


-- Loopy Registers Setters / Getters

-- TRAM
getTRAMData :: LOOPYFLAG -> StateT R2C02 IO Word8
getTRAMData L_COARSE_X = do
    loopy <- getTRAM
    let bits = fromIntegral . (shiftTake 0 5) $ loopy :: Word8
    return bits
getTRAMData L_COARSE_Y = do
    loopy <- getTRAM
    let bits = fromIntegral . (shiftTake 5 5) $ loopy :: Word8
    return bits
getTRAMData L_FINE_Y = do
    loopy <- getTRAM
    let bits = fromIntegral . (shiftTake 12 3)$ loopy :: Word8
    return bits
getTRAMData _ = error "Incorrect Flag"


setTRAMData :: LOOPYFLAG -> Word8 -> StateT R2C02 IO ()
setTRAMData L_COARSE_X val = do
    loopy <- getTRAM
    let loopy' = (loopy .&. 0xFFE0) + ((fromIntegral val) .&. 0x1F)
    setTRAM loopy'
setTRAMData L_COARSE_Y val = do
    loopy <- getTRAM
    let loopy' = (loopy .&. 0xFC1F) + (((fromIntegral val) .&. 0x1F) .<<. 5)
    setTRAM loopy'
setTRAMData L_FINE_Y val = do
    loopy <- getTRAM
    let loopy' = (loopy .&. 0x8fff) + ((shiftTake 0 3 (fromIntegral val) ) .<<. 12)
    setTRAM loopy'
setTRAMData _ _ = error "Incorrect Flag"


getTRAMBit :: LOOPYFLAG -> StateT R2C02 IO Bool
getTRAMBit L_NAMETABLE_X = b10 <$> getTRAM
getTRAMBit L_NAMETABLE_Y = b11 <$> getTRAM
getTRAMBit _ = error "Incorrect Flag"


setTRAMBit :: LOOPYFLAG -> Bool -> StateT R2C02 IO ()
setTRAMBit L_NAMETABLE_X v = mapTRAM (\tram -> if v then setBit tram 10 else clearBit tram 10)
setTRAMBit L_NAMETABLE_Y v = mapTRAM (\tram -> if v then setBit tram 11 else clearBit tram 11)
setTRAMBit _ _ = error "Incorrect Flag"




-- VRAM
getVRAMData :: LOOPYFLAG -> StateT R2C02 IO Word8
getVRAMData L_COARSE_X = do
    loopy <- getVRAM
    let bits = fromIntegral $ shiftTake 0 5 loopy :: Word8
    return bits
getVRAMData L_COARSE_Y = do
    loopy <- getVRAM
    let bits = fromIntegral $ shiftTake 5 5 loopy :: Word8
    return bits
getVRAMData L_FINE_Y = do
    loopy <- getVRAM
    let bits = fromIntegral $ shiftTake 12 3 loopy :: Word8
    return bits
getVRAMData _ = error "Incorrect Flag"


setVRAMData :: LOOPYFLAG -> Word8 -> StateT R2C02 IO ()
setVRAMData L_COARSE_X val = do
    loopy <- getVRAM
    let loopy' = (loopy .&. 0xffe0) + (shiftTake 0 5 (fromIntegral val))
    setVRAM loopy'
setVRAMData L_COARSE_Y val = do
    loopy <- getVRAM
    let loopy' = (loopy .&. 0xfc1f) + ((shiftTake 0 5 (fromIntegral val)) .<<. 5)
    setVRAM loopy'
setVRAMData L_FINE_Y val = do
    loopy <- getVRAM
    let loopy' = (loopy .&. 0x8fff) + ((shiftTake 0 3 (fromIntegral val) ) .<<. 12)
    setVRAM loopy'
setVRAMData _ _ = error "Incorrect Flag"


getVRAMBit :: LOOPYFLAG -> StateT R2C02 IO Bool
getVRAMBit L_NAMETABLE_X = b10 <$> getVRAM
getVRAMBit L_NAMETABLE_Y = b11 <$> getVRAM
getVRAMBit _ = error "Incorrect Flag"

setVRAMBit :: LOOPYFLAG -> Bool -> StateT R2C02 IO ()
setVRAMBit L_NAMETABLE_X v = mapVRAM (\tram -> if v then setBit tram 10 else clearBit tram 10)
setVRAMBit L_NAMETABLE_Y v = mapVRAM (\tram -> if v then setBit tram 11 else clearBit tram 11)
setVRAMBit _ _ = error "Incorrect Flag"



getScanline :: StateT R2C02 IO Int
getScanline = ppuScanline . context <$> get


getCycle :: StateT R2C02 IO Int
getCycle = ppuCycle . context <$> get


getNMI :: StateT R2C02 IO Bool
getNMI = ppuNMI . context <$> get


getComplete :: StateT R2C02 IO Bool
getComplete = complete . context <$> get


getShifterPatternLo :: StateT R2C02 IO Word16
getShifterPatternLo = shifterPatternLo . context <$> get


getShifterPatternHi :: StateT R2C02 IO Word16
getShifterPatternHi = shifterPatternHi . context <$> get


getShifterAttribLo :: StateT R2C02 IO Word16
getShifterAttribLo = shifterPatternLo . context <$> get


getShifterAttribHi :: StateT R2C02 IO Word16
getShifterAttribHi = shifterPatternHi . context <$> get


getNextTileLsb :: StateT R2C02 IO Word16
getNextTileLsb = nextTileLsb . context <$> get


getNextTileMsb :: StateT R2C02 IO Word16
getNextTileMsb = nextTileMsb . context <$> get


getNextTileID :: StateT R2C02 IO Word16
getNextTileID = nextTileID . context <$> get


getNextTileAttrib :: StateT R2C02 IO Word16
getNextTileAttrib = nextTileAttrib . context <$> get


getBGPixel :: StateT R2C02 IO Word8
getBGPixel = bgPixel . context <$> get


getBGPalette :: StateT R2C02 IO Word8
getBGPalette = bgPalette . context <$> get



setScanline :: Int -> StateT R2C02 IO ()
setScanline v = modify(\ppu -> ppu{context = (context ppu){ppuScanline = v}})


setCycle :: Int -> StateT R2C02 IO ()
setCycle v = modify(\ppu -> ppu{context = (context ppu){ppuCycle = v}})


setNMI :: Bool -> StateT R2C02 IO ()
setNMI v = modify(\ppu -> ppu{context = (context ppu){ppuNMI = v}})


setComplete :: Bool -> StateT R2C02 IO ()
setComplete v = modify(\ppu -> ppu{context = (context ppu){complete = v}})


setShifterPatternLo :: Word16 -> StateT R2C02 IO ()
setShifterPatternLo v = modify(\ppu -> ppu{context = (context ppu){shifterPatternLo = v}})


setShifterPatternHi :: Word16 -> StateT R2C02 IO ()
setShifterPatternHi v = modify(\ppu -> ppu{context = (context ppu){shifterPatternHi = v}})


setShifterAttribLo :: Word16 -> StateT R2C02 IO ()
setShifterAttribLo v = modify(\ppu -> ppu{context = (context ppu){shifterAttribLo = v}})


setShifterAttribHi :: Word16 -> StateT R2C02 IO ()
setShifterAttribHi v = modify(\ppu -> ppu{context = (context ppu){shifterAttribHi = v}})


setNextTileLsb :: Word16 -> StateT R2C02 IO ()
setNextTileLsb v = modify(\ppu -> ppu{context = (context ppu){nextTileLsb = v}})


setNextTileMsb :: Word16 -> StateT R2C02 IO ()
setNextTileMsb v = modify(\ppu -> ppu{context = (context ppu){nextTileMsb = v}})


setNextTileID :: Word16 -> StateT R2C02 IO ()
setNextTileID v = modify(\ppu -> ppu{context = (context ppu){nextTileID = v}})


setNextTileAttrib :: Word16 -> StateT R2C02 IO ()
setNextTileAttrib v = modify(\ppu -> ppu{context = (context ppu){nextTileAttrib = v}})


setBGPixel :: Word8 -> StateT R2C02 IO ()
setBGPixel v = modify(\ppu -> ppu{context = (context ppu){bgPixel = v}})


setBGPalette :: Word8 -> StateT R2C02 IO ()
setBGPalette v = modify(\ppu -> ppu{context = (context ppu){bgPalette = v}})



-- Interface

writeByte :: Word16 -> Word8 -> StateT R2C02 IO ()
writeByte addr byte = do
    ppu <- get
    let write = iWriteByte . interface $ ppu
    liftIO $ write addr byte


readByte ::Word16 -> StateT R2C02 IO Word8
readByte addr = do
    ppu <- get
    let read = iReadByte . interface $ ppu
    lift $ read addr


setPixel :: (Word16, Word16) -> Word8 -> StateT R2C02 IO ()
setPixel addr byte = do
    ppu <- get
    let draw = iSetPixel . interface $ ppu
    liftIO $ draw addr byte


triggerNMI :: StateT R2C02 IO ()
triggerNMI = do
    ppu <- get
    let trigger = iTriggerNMI . interface $ ppu
    liftIO $ trigger



-- CPU Interface

cpuRead :: Word16 -> StateT R2C02 IO Word8
cpuRead addr
    | addr == 0x0000 = return 0 -- Control: Write Only
    | addr == 0x0001 = return 0 -- Mask: Write Only
    | addr == 0x0002 = readStatus -- Status
    | addr == 0x0003 = return 0 -- OAM Address: Write Only
    | addr == 0x0004 = return 0 -- OAM Data: TODO
    | addr == 0x0005 = return 0 -- Scroll: Write Only
    | addr == 0x0006 = return 0 -- Address: WriteOnly
    | addr == 0x0007 = readData -- Data
    | otherwise = error "CPU Attempted to read out of turn"


readStatus :: StateT R2C02 IO Word8
readStatus = do
    buff <- getDataBuffer
    status <- getStatus
    let byte = (status .&. 0xE0) .|. (buff .&. 0x1F)
    setWriteToggle False
    setSTATUSFlag S_VERTICAL_BLANK False
    return $ byte


readData :: StateT R2C02 IO Word8
readData = do
    v <- getVRAM
    oldbuff <- getDataBuffer
    newbuff <- readByte v
    setDataBuffer newbuff
    let byte = if v >= 0x3F00 then newbuff else oldbuff
    increment_mode <- getCTRLFlag C_INCREMENT_MODE
    if increment_mode then setVRAM (v + 32) else setVRAM (v + 1)
    return byte


cpuWrite :: Word16 -> Word8 -> StateT R2C02 IO ()
cpuWrite addr byte
    | addr == 0x0000 = writeControl byte
    | addr == 0x0001 = writeMask byte
    | addr == 0x0002 = return () -- Status: Read Only
    | addr == 0x0003 = return () -- TODO: Implement OAS ADDR support
    | addr == 0x0004 = return () -- TODO: Implement OAS DATA support
    | addr == 0x0005 = writeScroll byte
    | addr == 0x0006 = writeAddress byte
    | addr == 0x0007 = writeData byte
    | otherwise = return () -- TODO: Log error


writeControl :: Word8 -> StateT R2C02 IO ()
writeControl byte = do
    setControl byte
    let nx = b0 byte
    let ny = b1 byte
    setTRAMBit L_NAMETABLE_X nx
    setTRAMBit L_NAMETABLE_Y ny


writeMask :: Word8 -> StateT R2C02 IO ()
writeMask byte = setMask byte


writeScroll :: Word8 -> StateT R2C02 IO ()
writeScroll byte = do
    firstWrite <- not <$> getWriteToggle
    if firstWrite
        then do
            setWriteToggle True
            setTRAMData L_COARSE_X (shiftTake 3 5 byte)
            setFineX (shiftTake 0 3 byte)
        else do
            setWriteToggle False
            setTRAMData L_COARSE_Y (shiftTake 3 5 byte)
            setTRAMData L_FINE_Y (shiftTake 0 3 byte)


writeAddress :: Word8 -> StateT R2C02 IO ()
writeAddress byte = do
    firstWrite <- not <$> getWriteToggle
    if firstWrite
        then do
            setWriteToggle True
            let fullbyte = fromIntegral byte :: Word16
            t <- getTRAM
            let t' = ((shiftTake 0 6 fullbyte) .<<. 8) .|. (t .&. 0x00FF)
            setTRAM t'
        else do
            setWriteToggle False
            let fullbyte = fromIntegral byte :: Word16
            t <- getTRAM
            let t' = (fullbyte) .|. (t .&. 0xFF00)
            setTRAM t'
            setVRAM t'


writeData :: Word8 -> StateT R2C02 IO ()
writeData byte = do
    v <- getVRAM
    writeByte v byte
    increment_mode <- getCTRLFlag C_INCREMENT_MODE
    if increment_mode then setVRAM (v + 32) else setVRAM (v + 1)


-- Background Tick


incCoarseX :: StateT R2C02 IO ()
incCoarseX = do
    rbgFlag <- getMASKFlag M_RENDER_BACKGROUND
    fsFlag <- getMASKFlag M_RENDER_SPRITES 

    when (rbgFlag || fsFlag) (do
        coarseX <- getVRAMData L_COARSE_X
        if coarseX < 31
            then setVRAMData L_COARSE_X (coarseX + 1)
            else do
                setVRAMData L_COARSE_X 0
                ntx <- getVRAMBit L_NAMETABLE_X
                setVRAMBit L_NAMETABLE_X (not ntx))


incCoarseY :: StateT R2C02 IO ()
incCoarseY = do
    coarseY <- getVRAMData L_COARSE_Y
    if coarseY == 29
        then do
            setVRAMData L_COARSE_Y 0
            (not <$> getVRAMBit L_NAMETABLE_Y) >>= (setVRAMBit L_NAMETABLE_Y)
        else
            if coarseY == 31
                then do
                    setVRAMData L_COARSE_Y 0
                else do
                    setVRAMData L_COARSE_Y (coarseY + 1)


incFineY :: StateT R2C02 IO ()
incFineY = do
    rbgFlag <- getMASKFlag M_RENDER_BACKGROUND
    fsFlag <- getMASKFlag M_RENDER_SPRITES 
    when (rbgFlag || fsFlag) (do
        cycle <- getCycle
        scanline <- getScanline
        fineY <- getVRAMData L_FINE_Y
        if fineY < 7
            then setVRAMData L_FINE_Y (fineY + 1)
            else do
                setVRAMData L_FINE_Y 0
                incCoarseY)


nametableBase :: Bool -> Bool -> Word16
nametableBase nx ny = x + y where
    x = if nx then 0x400 else 0
    y = if ny then 0x800 else 0


incScanline :: StateT R2C02 IO ()
incScanline = do
    scanline <- getScanline
    if scanline >= 260
    then do
        setScanline (-1)
        setComplete True
    else do
        setScanline (scanline + 1)


incCycle :: StateT R2C02 IO ()
incCycle = do
    cycle <- getCycle
    if cycle >= 340 
    then do
        setCycle 0
        incScanline
    else do
        setCycle (cycle + 1)


updateShifters :: StateT R2C02 IO ()
updateShifters = do
    (( .<<. 1) <$> getShifterAttribHi)  >>= setShifterAttribHi
    (( .<<. 1) <$> getShifterAttribLo)  >>= setShifterAttribLo
    (( .<<. 1) <$> getShifterPatternHi) >>= setShifterPatternHi
    (( .<<. 1) <$> getShifterPatternLo) >>= setShifterPatternLo


fetchNextTileID :: StateT R2C02 IO ()
fetchNextTileID = do
    vram <- getVRAM
    let addr = 0x2000 .|. (vram .&. 0x0FFF)
    byte <- readByte addr
    setNextTileID (fromIntegral byte)


getAttribInfo :: Word16 -> Word16 -> Word8 -> Word16
getAttribInfo tx ty byte = fromIntegral output where
    shiftX = if b1 tx then 0x2 else 0x0
    shiftY = if b1 ty then 0x4 else 0x0
    shift = shiftX + shiftY
    output = (byte .>>. shift) .&. 0x03


fetchNextTileAttrib :: StateT R2C02 IO ()
fetchNextTileAttrib = do
    nx <- getVRAMBit L_NAMETABLE_X
    ny <- getVRAMBit L_NAMETABLE_Y
    tx <- fromIntegral <$> getVRAMData L_COARSE_X
    ty <- fromIntegral <$> getVRAMData L_COARSE_Y
    let base = 0x23C0 + (nametableBase nx ny) 
    let offset = 8 * (ty .>>. 2) + (tx .>>. 2) :: Word16
    let addr = base + offset
    byte <- readByte addr
    setNextTileAttrib $ getAttribInfo tx ty byte


fetchNextTileLsb :: StateT R2C02 IO ()
fetchNextTileLsb = do
    ptrn <- getCTRLFlag C_PATTERN_BACKGROUND
    let base = if ptrn then 0x1000 else 0x0000 :: Word16
    ntID <- getNextTileID
    fineY <- fromIntegral <$> (getVRAMData L_FINE_Y)
    let addr = base + ntID * 16 + fineY
    byte <- fromIntegral <$> (readByte addr)
    setNextTileLsb byte


fetchNextTileMsb :: StateT R2C02 IO ()
fetchNextTileMsb = do
    ptrn <- getCTRLFlag C_PATTERN_BACKGROUND
    let base = if ptrn then 0x1000 else 0x0000
    ntID <- getNextTileID
    fineY <- fromIntegral <$> (getVRAMData L_FINE_Y)
    let addr = base + ntID * 16 + fineY + 8
    byte <- fromIntegral <$> (readByte addr)
    setNextTileMsb byte


fetchNextInfo :: Int -> StateT R2C02 IO ()
fetchNextInfo 0 = loadBackgroundShifters >> fetchNextTileID
fetchNextInfo 2 = fetchNextTileAttrib
fetchNextInfo 4 = fetchNextTileLsb
fetchNextInfo 6 = fetchNextTileMsb
fetchNextInfo 7 = incCoarseX
fetchNextInfo _ = return ()


loadBackgroundShifters :: StateT R2C02 IO ()
loadBackgroundShifters = do
    cShifterPatternLo  <- getShifterPatternLo
    cShifterPatternHi  <- getShifterPatternHi
    cNextTileLsb <- getNextTileLsb
    cNextTileMsb <- getNextTileMsb
    setShifterPatternLo (cNextTileLsb .|. (cShifterPatternLo .&. 0xFF00))
    setShifterPatternHi (cNextTileMsb .|. (cShifterPatternHi .&. 0xFF00))

    cShifterAttribLo  <- getShifterAttribLo
    cShifterAttribHi  <- getShifterAttribHi
    cNextTileAttrib <- getNextTileAttrib

    let eLo = if testBit cNextTileAttrib 0 then 0xFF else 0x00
    let eHi = if testBit cNextTileAttrib 1 then 0xFF else 0x00

    setShifterAttribLo (eLo .|. (cShifterAttribLo .&. 0xFF00))
    setShifterAttribHi (eHi .|. (cShifterAttribHi .&. 0xFF00))


transferX :: StateT R2C02 IO ()
transferX = do
    rbgFlag <- getMASKFlag M_RENDER_BACKGROUND
    fsFlag <- getMASKFlag M_RENDER_SPRITES 
    when (rbgFlag || fsFlag) (do
        getTRAMBit L_NAMETABLE_X >>= (setVRAMBit L_NAMETABLE_X)
        getTRAMData L_COARSE_X   >>= (setVRAMData L_COARSE_X)
        )


transferY :: StateT R2C02 IO ()
transferY = do
    rbgFlag <- getMASKFlag M_RENDER_BACKGROUND
    fsFlag <- getMASKFlag M_RENDER_SPRITES 
    when (rbgFlag || fsFlag) (do
        getTRAMData L_FINE_Y     >>= (setVRAMData L_FINE_Y)
        getTRAMData L_COARSE_Y   >>= (setVRAMData L_COARSE_Y)
        getTRAMBit L_NAMETABLE_Y >>= (setVRAMBit L_NAMETABLE_Y)
        )


handleVisibleScanline :: StateT R2C02 IO ()
handleVisibleScanline = do
    scanline <- getScanline
    cycle <- getCycle
    when (cycle == (-1) && cycle == 1)  (setSTATUSFlag S_VERTICAL_BLANK False)
    when (cycle >= 2    && cycle < 258) (updateShifters >> (fetchNextInfo (mod (cycle - 1) 8)))
    when (cycle >= 321  && cycle < 338) (updateShifters >> (fetchNextInfo (mod (cycle - 1) 8)))
    when (cycle == 256) incFineY
    when (cycle == 257) (loadBackgroundShifters >> transferX)
    when (cycle == 340) fetchNextTileID
    when (scanline == (-1) && cycle == 304) transferY -- TODO: THIS IS NOT ACCURATE. THIS HAPPENS FOR EVERY CYCLE BETWEEN 280 AND 304. BUT I THINK IT SHOULD BE FIEN


handleEndOfFrame :: StateT R2C02 IO ()
handleEndOfFrame = do
    scanline <- getScanline
    cycle <- getCycle
    when (scanline == 241 && cycle == 1) (do
        setSTATUSFlag S_VERTICAL_BLANK True
        enableNMI <- getCTRLFlag C_ENABLE_NMI
        when enableNMI triggerNMI
        )


handleComposition :: StateT R2C02 IO ()
handleComposition = do
    fineX <- fromIntegral <$> getFineX
    bgPatternLo <- getShifterPatternLo
    bgPatternHi <- getShifterPatternHi
    bgAttribLo <- getShifterAttribLo
    bgAttribHi <- getShifterAttribHi

    let mux = 0x8000 .>>. fineX

    let px0 = if (bgPatternLo .&. mux) > 0 then 0x1 else 0x0
    let px1 = if (bgPatternHi .&. mux) > 0 then 0x2 else 0x0
    let px = px0 + px1


    let pl0 = if (bgAttribLo .&. mux) > 0 then 0x1 else 0x0
    let pl1 = if (bgAttribHi .&. mux) > 0 then 0x2 else 0x0
    let pl = pl0 + pl1

    setBGPixel px
    setBGPalette pl


fetchColor :: StateT R2C02 IO Word8
fetchColor = do
    pixel <- fromIntegral <$> getBGPixel
    palette <- fromIntegral <$> getBGPalette
    let offset = 0x3F00
    let addr = offset + 4 * palette + pixel :: Word16
    byte <- readByte addr
    return byte


renderPixel :: StateT R2C02 IO ()
renderPixel = do
    cycle <- getCycle
    scanline <- getScanline
    color <- fetchColor
    setPixel (fromIntegral cycle - 1, fromIntegral scanline) color


renderScanline :: StateT R2C02 IO ()
renderScanline = do
    cycle <- getCycle
    when (cycle >= 0 && cycle < 256) renderPixel


skipFirstCycle :: StateT R2C02 IO ()
skipFirstCycle = do
    cycle <- getCycle
    when (cycle == 0) (setCycle 1)


tickBackground :: StateT R2C02 IO ()
tickBackground = do
    scanline <- getScanline
    when (scanline == 0) skipFirstCycle
    when (scanline >= (-1) && scanline < 240) handleVisibleScanline
    when (scanline >= 241  && scanline < 261) handleEndOfFrame
    renderBackground <- getMASKFlag M_RENDER_BACKGROUND
    when (renderBackground) handleComposition
    when (scanline >= 0 && scanline < 240) renderScanline
    incCycle


tick :: StateT R2C02 IO ()
tick = do
    tickBackground

