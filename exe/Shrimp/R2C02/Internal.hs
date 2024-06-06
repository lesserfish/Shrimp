module Shrimp.R2C02.Internal where

import qualified Shrimp.Display as Display
import qualified Shrimp.Memory as Memory
import Control.Monad (when)
import Control.Monad.State
import Data.Bits
import Data.Word
import Shrimp.Utils
import qualified Shrimp.MOS6502 as Display


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

data SPRITEFLAG
    = SPRITE_PALETTE
    | SPRITE_PRIORITY
    | SPRITE_HORIZONTAL_FLIP
    | SPRITE_VERTICAL_FLIP

data Context = Context
    { ppuNMI :: Bool
    , complete :: Bool
    , ppuScanline :: Int
    , ppuCycle :: Int
    , shifterData :: Word64
    , nextTile :: Word64
    , nextTileID :: Word16
    , nextTileAttrib :: Word16
    , bgPixel :: Word8
    , bgPalette :: Word8
    , oamAddress :: Word8
    }

data Registers = Registers
    { ppuctrl       :: Word8
    , ppumask       :: Word8
    , ppustatus     :: Word8
    , fineX         :: Int
    , ppuDataBuffer :: Word8
    , vram          :: Word16
    , tram          :: Word16
    , writeToggle   :: Bool
    }


data Interface = Interface
    { iReadByte :: Word16 -> IO Word8
    , iWriteByte :: Word16 -> Word8 -> IO ()
    , iSetPixel :: (Word16, Word16) -> Word8 -> IO ()
    , iPeekByte :: Word16 -> IO Word8
    }

data R2C02 = R2C02
    { registers :: Registers
    , context :: Context
    , interface :: Interface
    , oamData :: Memory.RAM
    , spriteBuffer :: Display.LineBuffer
    , backgroundBuffer :: Display.LineBuffer
    }

data Sprite = Sprite
    { sprY :: Word8
    , sprTile :: Word8
    , sprAttr :: Word8
    , sprX :: Word8
    }

getSprite :: Word16 -> StateT R2C02 IO Sprite
getSprite id = do
    let ya = 0x04 * id + 0x00
    let ta = 0x04 * id + 0x01
    let aa = 0x04 * id + 0x02
    let xa = 0x04 * id + 0x03
    oam <- getOAM
    y <- liftIO $ Memory.readByte oam ya
    t <- liftIO $ Memory.readByte oam ta
    a <- liftIO $ Memory.readByte oam aa
    x <- liftIO $ Memory.readByte oam xa
    return $ Sprite y t a x


-- Creation

new :: Interface -> IO R2C02
new interface = do
    let reg = Registers 0 0 0 0 0 0 0 False
    let ctx = Context False False 0 0 0 0 0 0 0 0 0
    oam <- Memory.new 0xFF 0xFF
    sb <- Display.newLineBuffer (32 * 8)
    bb <- Display.newLineBuffer (32 * 8 + 32)
    return $ R2C02 reg ctx interface oam sb bb

reset :: StateT R2C02 IO ()
reset = do
    ppu <- get
    ppu' <- liftIO $ new (interface ppu)
    put ppu'

-- Registers Setters / Getters

mapControl :: (Word8 -> Word8) -> StateT R2C02 IO ()
mapControl f = modify (\ppu -> ppu{registers = (registers ppu){ppuctrl = f . ppuctrl . registers $ ppu}})


mapMask :: (Word8 -> Word8) -> StateT R2C02 IO ()
mapMask f = modify (\ppu -> ppu {registers = (registers ppu){ppumask = f . ppumask . registers $ ppu}})


mapStatus :: (Word8 -> Word8) -> StateT R2C02 IO ()
mapStatus f = modify (\ppu -> ppu {registers = (registers ppu){ppustatus = f . ppustatus . registers $ ppu}})


mapFineX :: (Int -> Int) -> StateT R2C02 IO ()
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


setFineX :: Int -> StateT R2C02 IO ()
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


getFineX :: StateT R2C02 IO Int
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
getCTRLFlag C_NAMETABLE_X           = b0' <$> getControl
getCTRLFlag C_NAMETABLE_Y           = b1' <$> getControl
getCTRLFlag C_INCREMENT_MODE        = b2' <$> getControl
getCTRLFlag C_PATTERN_SPRITE        = b3' <$> getControl
getCTRLFlag C_PATTERN_BACKGROUND    = b4' <$> getControl
getCTRLFlag C_SPRITE_SIZE           = b5' <$> getControl
getCTRLFlag C_SLAVE_MDOE            = b6' <$> getControl
getCTRLFlag C_ENABLE_NMI            = b7' <$> getControl


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
getMASKFlag M_GRAYSCALE                 = b0' <$> getMask
getMASKFlag M_RENDER_BACKGROUND_LEFT    = b1' <$> getMask
getMASKFlag M_RENDER_SPRITES_LEFT       = b2' <$> getMask
getMASKFlag M_RENDER_BACKGROUND         = b3' <$> getMask
getMASKFlag M_RENDER_SPRITES            = b4' <$> getMask
getMASKFlag M_ENHANCE_RED               = b5' <$> getMask
getMASKFlag M_ENHANCE_GREEN             = b6' <$> getMask
getMASKFlag M_ENHANCE_BLUE              = b7' <$> getMask


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
getSTATUSFlag S_SPRITE_OVERFLOW     = b5' <$> getStatus
getSTATUSFlag S_SPRITE_ZERO_HIT     = b6' <$> getStatus
getSTATUSFlag S_VERTICAL_BLANK      = b7' <$> getStatus


setSTATUSFlag :: STATUSFLAG -> Bool -> StateT R2C02 IO ()
setSTATUSFlag S_SPRITE_OVERFLOW v       = mapStatus (\ctrl -> if v then setBit ctrl 5 else clearBit ctrl 5)
setSTATUSFlag S_SPRITE_ZERO_HIT v       = mapStatus (\ctrl -> if v then setBit ctrl 6 else clearBit ctrl 6)
setSTATUSFlag S_VERTICAL_BLANK v        = mapStatus (\ctrl -> if v then setBit ctrl 7 else clearBit ctrl 7)


-- Loopy Registers Setters / Getters

-- TRAM
getTRAMData :: LOOPYFLAG -> StateT R2C02 IO Word8
getTRAMData L_COARSE_X = do
    loopy <- getTRAM
    let bits = fromIntegral . (shiftTake2 0 5) $ loopy :: Word8
    return bits
getTRAMData L_COARSE_Y = do
    loopy <- getTRAM
    let bits = fromIntegral . (shiftTake2 5 5) $ loopy :: Word8
    return bits
getTRAMData L_FINE_Y = do
    loopy <- getTRAM
    let bits = fromIntegral . (shiftTake2 12 3)$ loopy :: Word8
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
    let loopy' = (loopy .&. 0x8fff) + ((shiftTake2 0 3 (fromIntegral val) ) .<<. 12)
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
    let bits = fromIntegral $ shiftTake2 0 5 loopy :: Word8
    return bits
getVRAMData L_COARSE_Y = do
    loopy <- getVRAM
    let bits = fromIntegral $ shiftTake2 5 5 loopy :: Word8
    return bits
getVRAMData L_FINE_Y = do
    loopy <- getVRAM
    let bits = fromIntegral $ shiftTake2 12 3 loopy :: Word8
    return bits
getVRAMData _ = error "Incorrect Flag"


setVRAMData :: LOOPYFLAG -> Word8 -> StateT R2C02 IO ()
setVRAMData L_COARSE_X val = do
    loopy <- getVRAM
    let loopy' = (loopy .&. 0xffe0) + (shiftTake2 0 5 (fromIntegral val))
    setVRAM loopy'
setVRAMData L_COARSE_Y val = do
    loopy <- getVRAM
    let loopy' = (loopy .&. 0xfc1f) + ((shiftTake2 0 5 (fromIntegral val)) .<<. 5)
    setVRAM loopy'
setVRAMData L_FINE_Y val = do
    loopy <- getVRAM
    let loopy' = (loopy .&. 0x8fff) + ((shiftTake2 0 3 (fromIntegral val) ) .<<. 12)
    setVRAM loopy'
setVRAMData _ _ = error "Incorrect Flag"


getVRAMBit :: LOOPYFLAG -> StateT R2C02 IO Bool
getVRAMBit L_NAMETABLE_X = b10 <$> getVRAM
getVRAMBit L_NAMETABLE_Y = b11 <$> getVRAM
getVRAMBit _ = error "Incorrect Flag"

setVRAMBit :: LOOPYFLAG -> Bool -> StateT R2C02 IO ()
setVRAMBit L_NAMETABLE_X v = mapVRAM (\vram -> if v then setBit vram 10 else clearBit vram 10)
setVRAMBit L_NAMETABLE_Y v = mapVRAM (\vram -> if v then setBit vram 11 else clearBit vram 11)
setVRAMBit _ _ = error "Incorrect Flag"

getSpriteBit :: SPRITEFLAG -> Sprite -> Bool
getSpriteBit SPRITE_PRIORITY sprite        = b5' . sprAttr $ sprite
getSpriteBit SPRITE_HORIZONTAL_FLIP sprite = b6' . sprAttr $ sprite
getSpriteBit SPRITE_VERTICAL_FLIP sprite   = b7' . sprAttr $ sprite

getSpritePalette :: Sprite -> Word8
getSpritePalette sprite = 4 + ((shiftTake1 0 2) . sprAttr $ sprite)

getScanline :: StateT R2C02 IO Int
getScanline = ppuScanline . context <$> get


getCycle :: StateT R2C02 IO Int
getCycle = ppuCycle . context <$> get


getNMI :: StateT R2C02 IO Bool
getNMI = ppuNMI . context <$> get


fetchNMI :: StateT R2C02 IO Bool
fetchNMI = do
    nmi <- getNMI
    setNMI False
    return nmi

getComplete :: StateT R2C02 IO Bool
getComplete = complete . context <$> get

fetchComplete :: StateT R2C02 IO Bool
fetchComplete = do
    done <- getComplete
    setComplete False
    return done


getShifterData :: StateT R2C02 IO Word64
getShifterData = shifterData . context <$> get


getNextTile :: StateT R2C02 IO Word64
getNextTile = nextTile . context <$> get

getNextTileID :: StateT R2C02 IO Word16
getNextTileID = nextTileID . context <$> get


getNextTileAttrib :: StateT R2C02 IO Word16
getNextTileAttrib = nextTileAttrib . context <$> get


getBGPixel :: StateT R2C02 IO Word8
getBGPixel = bgPixel . context <$> get


getBGPalette :: StateT R2C02 IO Word8
getBGPalette = bgPalette . context <$> get

getOAMAddress :: StateT R2C02 IO Word8
getOAMAddress = oamAddress . context <$> get


getOAM :: StateT R2C02 IO Memory.RAM
getOAM = oamData <$> get

getSpriteBuffer :: StateT R2C02 IO Display.LineBuffer
getSpriteBuffer = spriteBuffer <$> get

getBackgroundBuffer :: StateT R2C02 IO Display.LineBuffer
getBackgroundBuffer = backgroundBuffer <$> get


setScanline :: Int -> StateT R2C02 IO ()
setScanline v = modify(\ppu -> ppu{context = (context ppu){ppuScanline = v}})


setCycle :: Int -> StateT R2C02 IO ()
setCycle v = modify(\ppu -> ppu{context = (context ppu){ppuCycle = v}})


setNMI :: Bool -> StateT R2C02 IO ()
setNMI v = modify(\ppu -> ppu{context = (context ppu){ppuNMI = v}})


setComplete :: Bool -> StateT R2C02 IO ()
setComplete v = modify(\ppu -> ppu{context = (context ppu){complete = v}})


setShifterData :: Word64 -> StateT R2C02 IO ()
setShifterData v = modify(\ppu -> ppu{context = (context ppu){shifterData = v}})


setNextTileLsb :: Word16 -> StateT R2C02 IO ()
setNextTileLsb v = do
    ntile <- nextTile . context <$> get
    let ntile' = (ntile .&. 0xFFFFFFFFFFFFFF00) .|. (fromIntegral v)
    modify(\ppu -> ppu{context = (context ppu){nextTile = ntile'}})


setNextTileMsb :: Word16 -> StateT R2C02 IO ()
setNextTileMsb v = do
    ntile <- nextTile . context <$> get
    let ntile' = (ntile .&. 0xFFFFFFFFFF00FFFF) .|. (takeShift8 8 16 (fromIntegral v))
    modify(\ppu -> ppu{context = (context ppu){nextTile = ntile'}})


setNextTileID :: Word16 -> StateT R2C02 IO ()
setNextTileID v = modify(\ppu -> ppu{context = (context ppu){nextTileID = v}})


setNextTileAttrib :: Word16 -> StateT R2C02 IO ()
setNextTileAttrib v = modify(\ppu -> ppu{context = (context ppu){nextTileAttrib = v}})


setBGPixel :: Word8 -> StateT R2C02 IO ()
setBGPixel v = modify(\ppu -> ppu{context = (context ppu){bgPixel = v}})


setBGPalette :: Word8 -> StateT R2C02 IO ()
setBGPalette v = modify(\ppu -> ppu{context = (context ppu){bgPalette = v}})

setOAMAddress :: Word8 -> StateT R2C02 IO ()
setOAMAddress v = modify(\ppu -> ppu{context = (context ppu){oamAddress = v}})


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

peekByte :: Word16 -> StateT R2C02 IO Word8
peekByte addr = do
    ppu <- get
    let peek = iPeekByte . interface $ ppu
    lift $ peek addr


setPixel :: (Word16, Word16) -> Word8 -> StateT R2C02 IO ()
setPixel addr byte = do
    ppu <- get
    let draw = iSetPixel . interface $ ppu
    liftIO $ draw addr byte


triggerNMI :: StateT R2C02 IO ()
triggerNMI = setNMI True



-- CPU Interface

cpuRead :: Word16 -> StateT R2C02 IO Word8
cpuRead addr
    | addr == 0x0000 = return 0 -- Control: Write Only
    | addr == 0x0001 = return 0 -- Mask: Write Only
    | addr == 0x0002 = readStatus -- Status
    | addr == 0x0003 = return 0 -- OAM Address: Write Only
    | addr == 0x0004 = readOAMData -- OAM Data: TODO
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


cpuPeek :: Word16 -> R2C02 -> IO Word8
cpuPeek addr ppu
    | addr == 0x0000 = return 0 -- Control: Write Only
    | addr == 0x0001 = return 0 -- Mask: Write Only
    | addr == 0x0002 = fst <$> (runStateT peekStatus ppu) -- Status
    | addr == 0x0003 = return 0 -- OAM Address: Write Only
    | addr == 0x0004 = fst <$> (runStateT readOAMData ppu) -- OAM Data
    | addr == 0x0005 = return 0 -- Scroll: Write Only
    | addr == 0x0006 = return 0 -- Address: WriteOnly
    | addr == 0x0007 = fst <$> (runStateT peekData ppu)-- Data
    | otherwise = error "CPU Attempted to read out of turn"

readOAMData :: StateT R2C02 IO Word8
readOAMData = do
    oam <- getOAM
    addr <- fromIntegral <$> getOAMAddress
    liftIO $ Memory.readByte oam addr

peekStatus :: StateT R2C02 IO Word8
peekStatus = do
    buff <- getDataBuffer
    status <- getStatus
    let byte = (status .&. 0xE0) .|. (buff .&. 0x1F)
    return $ byte


peekData :: StateT R2C02 IO Word8
peekData = do
    v <- getVRAM
    oldbuff <- getDataBuffer
    newbuff <- readByte v
    let byte = if v >= 0x3F00 then newbuff else oldbuff
    return byte
    

cpuWrite :: Word16 -> Word8 -> StateT R2C02 IO ()
cpuWrite addr byte
    | addr == 0x0000 = writeControl byte
    | addr == 0x0001 = writeMask byte
    | addr == 0x0002 = return () -- Status: Read Only
    | addr == 0x0003 = writeOAMAddress byte
    | addr == 0x0004 = writeOAMData byte
    | addr == 0x0005 = writeScroll byte
    | addr == 0x0006 = writeAddress byte
    | addr == 0x0007 = writeData byte
    | otherwise = return () -- TODO: Log error


writeControl :: Word8 -> StateT R2C02 IO ()
writeControl byte = do
    setControl byte
    let nx = b0' byte
    let ny = b1' byte
    setTRAMBit L_NAMETABLE_X nx
    setTRAMBit L_NAMETABLE_Y ny


writeMask :: Word8 -> StateT R2C02 IO ()
writeMask byte = setMask byte

writeOAMAddress :: Word8 -> StateT R2C02 IO ()
writeOAMAddress = setOAMAddress

writeOAMData :: Word8 -> StateT R2C02 IO ()
writeOAMData byte = do
    oam <- getOAM
    addr <- fromIntegral <$> getOAMAddress
    liftIO $ Memory.writeByte oam addr byte


writeScroll :: Word8 -> StateT R2C02 IO ()
writeScroll byte = do
    firstWrite <- not <$> getWriteToggle
    if firstWrite
        then do
            setWriteToggle True
            setTRAMData L_COARSE_X (shiftTake1 3 5 byte)
            setFineX (fromIntegral $ shiftTake1 0 3 byte)
        else do
            setWriteToggle False
            setTRAMData L_COARSE_Y (shiftTake1 3 5 byte)
            setTRAMData L_FINE_Y (shiftTake1 0 3 byte)


writeAddress :: Word8 -> StateT R2C02 IO ()
writeAddress byte = do
    firstWrite <- not <$> getWriteToggle
    if firstWrite
        then do
            setWriteToggle True
            let fullbyte = fromIntegral byte :: Word16
            t <- getTRAM
            let t' = ((shiftTake2 0 6 fullbyte) .<<. 8) .|. (t .&. 0x00FF)
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


dmaPort :: Word16 -> Word8 -> StateT R2C02 IO ()
dmaPort addr byte = do
    oam <- getOAM
    liftIO $ Memory.writeByte oam addr byte

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

-- Increases the current scanline, looping around if needed.
incScanline :: StateT R2C02 IO ()
incScanline = do
    scanline <- getScanline
    if scanline >= 261
    then do
        setScanline 0
        setComplete True
    else do
        setScanline (scanline + 1)


-- Increases the current cycle, looping around if needed
incCycle :: StateT R2C02 IO ()
incCycle = do
    cycle <- getCycle
    if cycle >= 340 
    then do
        setCycle 0
        incScanline
    else do
        setCycle (cycle + 1)

-- Gets the nametable base address
nametableBase :: Bool -> Bool -> Word16
nametableBase nx ny = x + y where
    x = if nx then 0x400 else 0
    y = if ny then 0x800 else 0


-- Transfers the X information from TRAM to VRAM
transferX :: StateT R2C02 IO ()
transferX = do
    rbgFlag <- getMASKFlag M_RENDER_BACKGROUND
    fsFlag <- getMASKFlag M_RENDER_SPRITES 
    when (rbgFlag || fsFlag) (do
        getTRAMBit L_NAMETABLE_X >>= (setVRAMBit L_NAMETABLE_X)
        getTRAMData L_COARSE_X   >>= (setVRAMData L_COARSE_X)
        )


-- Transfers the Y information from the TRAM to VRAM
transferY :: StateT R2C02 IO ()
transferY = do
    rbgFlag <- getMASKFlag M_RENDER_BACKGROUND
    fsFlag <- getMASKFlag M_RENDER_SPRITES 
    when (rbgFlag || fsFlag) (do
        getTRAMData L_FINE_Y     >>= (setVRAMData L_FINE_Y)
        getTRAMData L_COARSE_Y   >>= (setVRAMData L_COARSE_Y)
        getTRAMBit L_NAMETABLE_Y >>= (setVRAMBit L_NAMETABLE_Y)
        )

