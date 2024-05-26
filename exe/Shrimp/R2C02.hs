{-# LANGUAGE FlexibleContexts #-}

module Shrimp.R2C02 (
    R2C02 (..),
    setDEBUG,
    Context(..),
    Registers(..),
    r2c02,
    mDispatcher,
    tick,
    setNMI,
    reset,
    cpuRead,
    cpuWrite,
    nametableBase,
) where

import Control.Monad (when)
import Control.Monad.State
import Data.Bits
import Data.Word
import Shrimp.AbstractBus
import Shrimp.Cartridge (Mirroring(..))
import qualified Shrimp.Memory as Memory

-- Registers

data REGISTER
    = PPUCTRL
    | PPUMASK
    | PPUSTATUS
    | FINEX
    | VRAM
    | TRAM

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

data Registers = Registers
    { ppuctrl :: Word8
    , ppumask :: Word8
    , ppustatus :: Word8
    , fineX :: Word8
    , vram :: Word16
    , tram :: Word16
    , writeToggle :: Bool
    , ppuDataBuffer :: Word8
    }

class RegisterType a where
    getter :: REGISTER -> Registers -> a
    setter :: REGISTER -> a -> Registers -> Registers

instance RegisterType Word16 where
    getter VRAM = vram
    getter TRAM = tram
    getter _ = error "Attempted to read Word8 as Word16"

    setter VRAM val regs = regs{vram = val}
    setter TRAM val regs = regs{tram = val}
    setter _ _ _ = error "Attempted to write Word8 to Word16"

instance RegisterType Word8 where
    getter PPUCTRL = ppuctrl
    getter PPUMASK = ppumask
    getter PPUSTATUS = ppustatus
    getter FINEX = fineX
    getter _ = error "Attempted to read Word16 as Word8"

    setter PPUCTRL val regs = regs{ppuctrl = val}
    setter PPUMASK val regs = regs{ppumask = val}
    setter PPUSTATUS val regs = regs{ppustatus = val}
    setter FINEX val regs = regs{fineX = val}
    setter _ _ _ = error "Attempted to write Word16 to Word8"

modifyFst :: (Monad m) => (a -> a) -> StateT (a, b) m ()
modifyFst f = modify(\(a, b) -> (f a, b))

mDispatcher :: (PBus m ()) => StateT (R2C02, ()) m b -> R2C02 -> m (R2C02, b)
mDispatcher state ppu = do
    (output, (ppu', _)) <- runStateT state (ppu, ())
    return $ (ppu', output)

mapReg :: (PBus m a, RegisterType b) => REGISTER -> (b -> b) -> StateT (R2C02, a) m ()
mapReg register f = modifyFst updateppu where
    updateppu ppu = ppu{registers = setter register (updatereg $ ppu) (registers ppu)}
    updatereg ppu = f . (getter register) . registers $ ppu

setReg :: (PBus m a, RegisterType b) => REGISTER -> b -> StateT (R2C02, a) m ()
setReg reg value = mapReg reg (\_ -> value)

setRegIf :: (PBus m a, RegisterType b) => Bool -> REGISTER -> b -> StateT (R2C02, a) m ()
setRegIf condition reg value = if condition then setReg reg value else return ()

getReg :: (PBus m a, RegisterType b) => REGISTER -> StateT (R2C02, a) m b
getReg reg = (getter reg) . registers . fst <$> get


getCTRLFlag :: (PBus m a) => CTRLFLAG -> StateT (R2C02, a) m Bool
getCTRLFlag C_NAMETABLE_X = b0 <$> (getReg PPUCTRL :: (PBus m a) => StateT (R2C02, a) m Word8)
getCTRLFlag C_NAMETABLE_Y = b1 <$> (getReg PPUCTRL :: (PBus m a) => StateT (R2C02, a) m Word8)
getCTRLFlag C_INCREMENT_MODE = b2 <$> (getReg PPUCTRL :: (PBus m a) => StateT (R2C02, a) m Word8)
getCTRLFlag C_PATTERN_SPRITE = b3 <$> (getReg PPUCTRL :: (PBus m a) => StateT (R2C02, a) m Word8)
getCTRLFlag C_PATTERN_BACKGROUND = b4 <$> (getReg PPUCTRL :: (PBus m a) => StateT (R2C02, a) m Word8)
getCTRLFlag C_SPRITE_SIZE = b5 <$> (getReg PPUCTRL :: (PBus m a) => StateT (R2C02, a) m Word8)
getCTRLFlag C_SLAVE_MDOE = b6 <$> (getReg PPUCTRL :: (PBus m a) => StateT (R2C02, a) m Word8)
getCTRLFlag C_ENABLE_NMI = b7 <$> (getReg PPUCTRL :: (PBus m a) => StateT (R2C02, a) m Word8)

setCTRLFlag :: (PBus m a) => CTRLFLAG -> Bool -> StateT (R2C02, a) m ()
setCTRLFlag C_NAMETABLE_X v = mapReg PPUCTRL (\ctrl -> if v then (setBit ctrl 0) :: Word8 else (clearBit ctrl 0) :: Word8)
setCTRLFlag C_NAMETABLE_Y v = mapReg PPUCTRL (\ctrl -> if v then (setBit ctrl 1) :: Word8 else (clearBit ctrl 1) :: Word8)
setCTRLFlag C_INCREMENT_MODE v = mapReg PPUCTRL (\ctrl -> if v then (setBit ctrl 2) :: Word8 else (clearBit ctrl 2) :: Word8)
setCTRLFlag C_PATTERN_SPRITE v = mapReg PPUCTRL (\ctrl -> if v then (setBit ctrl 3) :: Word8 else (clearBit ctrl 3) :: Word8)
setCTRLFlag C_PATTERN_BACKGROUND v = mapReg PPUCTRL (\ctrl -> if v then (setBit ctrl 4) :: Word8 else (clearBit ctrl 4) :: Word8)
setCTRLFlag C_SPRITE_SIZE v = mapReg PPUCTRL (\ctrl -> if v then (setBit ctrl 5) :: Word8 else (clearBit ctrl 5) :: Word8)
setCTRLFlag C_SLAVE_MDOE v = mapReg PPUCTRL (\ctrl -> if v then (setBit ctrl 6) :: Word8 else (clearBit ctrl 6) :: Word8)
setCTRLFlag C_ENABLE_NMI v = mapReg PPUCTRL (\ctrl -> if v then (setBit ctrl 7) :: Word8 else (clearBit ctrl 7) :: Word8)

getMASKFlag :: (PBus m a) => MASKFLAG -> StateT (R2C02, a) m Bool
getMASKFlag M_GRAYSCALE = b0 <$> (getReg PPUMASK :: (PBus m a) => StateT (R2C02, a) m Word8)
getMASKFlag M_RENDER_BACKGROUND_LEFT = b1 <$> (getReg PPUMASK :: (PBus m a) => StateT (R2C02, a) m Word8)
getMASKFlag M_RENDER_SPRITES_LEFT = b2 <$> (getReg PPUMASK :: (PBus m a) => StateT (R2C02, a) m Word8)
getMASKFlag M_RENDER_BACKGROUND = b3 <$> (getReg PPUMASK :: (PBus m a) => StateT (R2C02, a) m Word8)
getMASKFlag M_RENDER_SPRITES = b4 <$> (getReg PPUMASK :: (PBus m a) => StateT (R2C02, a) m Word8)
getMASKFlag M_ENHANCE_RED = b5 <$> (getReg PPUMASK :: (PBus m a) => StateT (R2C02, a) m Word8)
getMASKFlag M_ENHANCE_GREEN = b6 <$> (getReg PPUMASK :: (PBus m a) => StateT (R2C02, a) m Word8)
getMASKFlag M_ENHANCE_BLUE = b7 <$> (getReg PPUMASK :: (PBus m a) => StateT (R2C02, a) m Word8)

setMASKFlag :: (PBus m a) => MASKFLAG -> Bool -> StateT (R2C02, a) m ()
setMASKFlag M_GRAYSCALE v = mapReg PPUMASK (\ctrl -> if v then (setBit ctrl 0) :: Word8 else (clearBit ctrl 0) :: Word8)
setMASKFlag M_RENDER_BACKGROUND_LEFT v = mapReg PPUMASK (\ctrl -> if v then (setBit ctrl 1) :: Word8 else (clearBit ctrl 1) :: Word8)
setMASKFlag M_RENDER_SPRITES_LEFT v = mapReg PPUMASK (\ctrl -> if v then (setBit ctrl 2) :: Word8 else (clearBit ctrl 2) :: Word8)
setMASKFlag M_RENDER_BACKGROUND v = mapReg PPUMASK (\ctrl -> if v then (setBit ctrl 3) :: Word8 else (clearBit ctrl 3) :: Word8)
setMASKFlag M_RENDER_SPRITES v = mapReg PPUMASK (\ctrl -> if v then (setBit ctrl 4) :: Word8 else (clearBit ctrl 4) :: Word8)
setMASKFlag M_ENHANCE_RED v = mapReg PPUMASK (\ctrl -> if v then (setBit ctrl 5) :: Word8 else (clearBit ctrl 5) :: Word8)
setMASKFlag M_ENHANCE_GREEN v = mapReg PPUMASK (\ctrl -> if v then (setBit ctrl 6) :: Word8 else (clearBit ctrl 6) :: Word8)
setMASKFlag M_ENHANCE_BLUE v = mapReg PPUMASK (\ctrl -> if v then (setBit ctrl 7) :: Word8 else (clearBit ctrl 7) :: Word8)

getSTATUSFlag :: (PBus m a) => STATUSFLAG -> StateT (R2C02, a) m Bool
getSTATUSFlag S_SPRITE_OVERFLOW = b5 <$> (getReg PPUSTATUS :: (PBus m a) => StateT (R2C02, a) m Word8)
getSTATUSFlag S_SPRITE_ZERO_HIT = b6 <$> (getReg PPUSTATUS :: (PBus m a) => StateT (R2C02, a) m Word8)
getSTATUSFlag S_VERTICAL_BLANK = b7 <$> (getReg PPUSTATUS :: (PBus m a) => StateT (R2C02, a) m Word8)

setSTATUSFlag :: (PBus m a) => STATUSFLAG -> Bool -> StateT (R2C02, a) m ()
setSTATUSFlag S_SPRITE_OVERFLOW v = mapReg PPUSTATUS (\ctrl -> if v then (setBit ctrl 5) :: Word8 else (clearBit ctrl 5) :: Word8)
setSTATUSFlag S_SPRITE_ZERO_HIT v = mapReg PPUSTATUS (\ctrl -> if v then (setBit ctrl 6) :: Word8 else (clearBit ctrl 6) :: Word8)
setSTATUSFlag S_VERTICAL_BLANK v = mapReg PPUSTATUS (\ctrl -> if v then (setBit ctrl 7) :: Word8 else (clearBit ctrl 7) :: Word8)

getTRAMData :: (PBus m a) => LOOPYFLAG -> StateT (R2C02, a) m Word8
getTRAMData L_COARSE_X = do
    loopy <- getReg TRAM :: (PBus m a) => StateT (R2C02, a) m Word16
    let bits = fromIntegral . (shiftTake 0 5) $ loopy :: Word8
    return bits
getTRAMData L_COARSE_Y = do
    loopy <- getReg TRAM :: (PBus m a) => StateT (R2C02, a) m Word16
    let bits = fromIntegral . (shiftTake 5 5) $ loopy :: Word8
    return bits
getTRAMData L_FINE_Y = do
    loopy <- getReg TRAM :: (PBus m a) => StateT (R2C02, a) m Word16
    let bits = fromIntegral . (shiftTake 12 3)$ loopy :: Word8
    return bits
getTRAMData _ = error "Incorrect Flag"

setTRAMData :: (PBus m a) => LOOPYFLAG -> Word8 -> StateT (R2C02, a) m ()
setTRAMData L_COARSE_X val = do
    loopy <- getReg TRAM :: (PBus m a) => StateT (R2C02, a) m Word16
    let loopy' = (loopy .&. 0xFFE0) + ((fromIntegral val) .&. 0x1F)
    setReg TRAM loopy' :: (PBus m a) => StateT (R2C02, a) m ()
setTRAMData L_COARSE_Y val = do
    loopy <- getReg TRAM :: (PBus m a) => StateT (R2C02, a) m Word16
    let loopy' = (loopy .&. 0xFC1F) + shiftL ((fromIntegral val) .&. 0x1F) 5
    setReg TRAM loopy' :: (PBus m a) => StateT (R2C02, a) m ()
setTRAMData L_FINE_Y val = do
    loopy <- getReg TRAM :: (PBus m a) => StateT (R2C02, a) m Word16
    let loopy' = (loopy .&. 0x47FF) + shiftL ((fromIntegral val) .&. 0x7) 12
    setReg TRAM loopy' :: (PBus m a) => StateT (R2C02, a) m ()
setTRAMData _ _ = error "Incorrect Flag"

getVRAMData :: (PBus m a) => LOOPYFLAG -> StateT (R2C02, a) m Word8
getVRAMData L_COARSE_X = do
    loopy <- getReg VRAM :: (PBus m a) => StateT (R2C02, a) m Word16
    let bits = fromIntegral $ shiftTake 0 5 loopy :: Word8
    return bits
getVRAMData L_COARSE_Y = do
    loopy <- getReg VRAM :: (PBus m a) => StateT (R2C02, a) m Word16
    let bits = fromIntegral $ shiftTake 5 5 loopy :: Word8
    return bits
getVRAMData L_FINE_Y = do
    loopy <- getReg VRAM :: (PBus m a) => StateT (R2C02, a) m Word16
    let bits = fromIntegral $ shiftTake 12 3 loopy :: Word8
    return bits
getVRAMData _ = error "Incorrect Flag"

setVRAMData :: (PBus m a) => LOOPYFLAG -> Word8 -> StateT (R2C02, a) m ()
setVRAMData L_COARSE_X val = do
    loopy <- getReg VRAM :: (PBus m a) => StateT (R2C02, a) m Word16
    let loopy' = (loopy .&. 0xffe0) + ((fromIntegral val) .&. 0x1F)
    setReg VRAM loopy' :: (PBus m a) => StateT (R2C02, a) m ()
setVRAMData L_COARSE_Y val = do
    loopy <- getReg VRAM :: (PBus m a) => StateT (R2C02, a) m Word16
    let loopy' = (loopy .&. 0xfc1f) + shiftL ((fromIntegral val) .&. 0x1F) 5
    setReg VRAM loopy' :: (PBus m a) => StateT (R2C02, a) m ()
setVRAMData L_FINE_Y val = do
    loopy <- getReg VRAM :: (PBus m a) => StateT (R2C02, a) m Word16
    let loopy' = (loopy .&. 0x47FF) + shiftL ((fromIntegral val) .&. 0x7) 12
    setReg VRAM loopy' :: (PBus m a) => StateT (R2C02, a) m ()
setVRAMData _ _ = error "Incorrect Flag"

getTRAMBit :: (PBus m a) => LOOPYFLAG -> StateT (R2C02, a) m Bool
getTRAMBit L_NAMETABLE_X = b10 <$> (getReg TRAM :: (PBus m a) => StateT (R2C02, a) m Word16)
getTRAMBit L_NAMETABLE_Y = b11 <$> (getReg TRAM :: (PBus m a) => StateT (R2C02, a) m Word16)
getTRAMBit _ = error "Incorrect Flag"

setTRAMBit :: (PBus m a) => LOOPYFLAG -> Bool -> StateT (R2C02, a) m ()
setTRAMBit L_NAMETABLE_X v = mapReg TRAM (\tram -> if v then (setBit tram 10) :: Word16 else (clearBit tram 10) :: Word16)
setTRAMBit L_NAMETABLE_Y v = mapReg TRAM (\tram -> if v then (setBit tram 11) :: Word16 else (clearBit tram 11) :: Word16)
setTRAMBit _ _ = error "Incorrect Flag"

getVRAMBit :: (PBus m a) => LOOPYFLAG -> StateT (R2C02, a) m Bool
getVRAMBit L_NAMETABLE_X = b10 <$> (getReg VRAM :: (PBus m a) => StateT (R2C02, a) m Word16)
getVRAMBit L_NAMETABLE_Y = b11 <$> (getReg VRAM :: (PBus m a) => StateT (R2C02, a) m Word16)
getVRAMBit _ = error "Incorrect Flag"

setVRAMBit :: (PBus m a) => LOOPYFLAG -> Bool -> StateT (R2C02, a) m ()
setVRAMBit L_NAMETABLE_X v = mapReg VRAM (\tram -> if v then (setBit tram 10) :: Word16 else (clearBit tram 10) :: Word16)
setVRAMBit L_NAMETABLE_Y v = mapReg VRAM (\tram -> if v then (setBit tram 11) :: Word16 else (clearBit tram 11) :: Word16)
setVRAMBit _ _ = error "Incorrect Flag"

getWriteToggle :: (PBus m a) => StateT (R2C02, a) m Bool
getWriteToggle = (writeToggle . registers . fst) <$> get

setWriteToggle :: (PBus m a) => Bool -> StateT (R2C02, a) m ()
setWriteToggle toggle = do
    (ppu, bus) <- get
    let reg = registers ppu
    let reg' = reg{writeToggle = toggle}
    let ppu' = ppu{registers = reg'}
    put (ppu', bus)

getDataBuffer :: (PBus m a) => StateT (R2C02, a) m Word8
getDataBuffer = (ppuDataBuffer . registers . fst) <$> get

setDataBuffer :: (PBus m a) => Word8 -> StateT (R2C02, a) m ()
setDataBuffer byte = do
    (ppu, bus) <- get
    let reg = registers ppu
    let reg' = reg{ppuDataBuffer = byte}
    let ppu' = ppu{registers = reg'}
    put (ppu', bus)

getFineX :: (PBus m a) => StateT (R2C02, a) m Word8
getFineX = (getReg FINEX :: (PBus m a) => StateT (R2C02, a) m Word8)

setFineX :: (PBus m a) => Word8 -> StateT (R2C02, a) m ()
setFineX byte = setReg FINEX byte :: (PBus m a) => StateT (R2C02, a) m ()

getTRAM :: (PBus m a) => StateT (R2C02, a) m Word16
getTRAM = (getReg TRAM :: (PBus m a) => StateT (R2C02, a) m Word16)

getVRAM :: (PBus m a) => StateT (R2C02, a) m Word16
getVRAM = (getReg VRAM :: (PBus m a) => StateT (R2C02, a) m Word16)

setTRAM :: (PBus m a) => Word16 -> StateT (R2C02, a) m ()
setTRAM t = setReg TRAM t :: (PBus m a) => StateT (R2C02, a) m ()

setVRAM :: (PBus m a) => Word16 -> StateT (R2C02, a) m ()
setVRAM t = do
    setReg VRAM t :: (PBus m a) => StateT (R2C02, a) m ()

shiftTake :: (Bits a, Integral b, Num a) => Int -> b -> a -> a
shiftTake s t x = (shiftR x s) .&. (2 ^ t - 1)

b0 x = testBit x 0

b1 x = testBit x 1

b2 x = testBit x 2

b3 x = testBit x 3

b4 x = testBit x 4

b5 x = testBit x 5

b6 x = testBit x 6

b7 x = testBit x 7

b8 x = testBit x 8

b9 x = testBit x 9

b10 x = testBit x 10

b11 x = testBit x 11

b12 x = testBit x 12

b13 x = testBit x 13

b14 x = testBit x 14

b15 x = testBit x 15

data Context = Context
    { ppuNMI :: Bool
    , ppuScanline :: Int
    , ppuCycle :: Int
    , complete :: Bool
    , cdebug :: String
    }

-- R2C02
data R2C02 = R2C02
    { registers :: Registers
    , context :: Context
    }

reset :: (PBus m a) => StateT (R2C02, a) m ()
reset = do
    (r2c02, bus) <- get
    let reg = Registers { ppuctrl = 0
    , ppumask = 0
    , ppustatus  = 0
    , fineX = 0
    , vram = 0
    , tram = 0
    , writeToggle = False
    , ppuDataBuffer = 0
    }
    let ctx = Context {ppuNMI = False
    , ppuScanline = -1
    , ppuCycle = 0
    , complete = False
    , cdebug = ""}
    let r2c02' = r2c02 {context = ctx, registers = reg}
    put (r2c02', bus)

r2c02 :: R2C02
r2c02 =
    R2C02
        { registers = reg
        , context = ctx
        }
  where
    reg = Registers { ppuctrl = 0
    , ppumask = 0
    , ppustatus  = 0
    , fineX = 0
    , vram = 0
    , tram = 0
    , writeToggle = False
    , ppuDataBuffer = 0
    }
    ctx = Context {ppuNMI = False
    , ppuScanline = -1
    , ppuCycle = 0
    , complete = False
    , cdebug = ""}


readByte :: (PBus m a) => Word16 -> StateT (R2C02, a) m Word8
readByte addr = do
    (ppu, bus) <- get
    (bus', byte) <- lift $ pReadByte addr bus
    put (ppu, bus')
    return byte

writeByte :: (PBus m a) => Word16 -> Word8 -> StateT (R2C02, a) m ()
writeByte addr byte = do
    (ppu, bus) <- get
    bus' <- lift $ pWriteByte addr byte bus
    put (ppu, bus')

debug :: (PBus m a) => String -> StateT (R2C02, a) m ()
debug str = do
    (ppu, bus) <- get
    lift $ pDebug str bus
    return ()


-- CPU Interface
cpuRead :: (PBus m a) => Word16 -> StateT (R2C02, a) m Word8
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

readStatus :: (PBus m a) => StateT (R2C02, a) m Word8
readStatus = do
    setSTATUSFlag S_VERTICAL_BLANK True
    buf <- getDataBuffer
    status <- getReg PPUSTATUS :: (PBus m a) => StateT (R2C02, a) m Word8
    let byte = (status .&. 0xE0) .|. (buf .&. 0x1F)
    setWriteToggle False
    setSTATUSFlag S_VERTICAL_BLANK False
    return $ byte

readData :: (PBus m a) => StateT (R2C02, a) m Word8
readData = do
    v <- getVRAM
    oldbuf <- getDataBuffer
    newbuf <- readByte v
    setDataBuffer newbuf
    let byte = if v >= 0x3F00 then newbuf else oldbuf
    increment_mode <- getCTRLFlag C_INCREMENT_MODE
    if increment_mode then setVRAM (v + 32) else setVRAM (v + 1)
    return byte

cpuWrite :: (PBus m a) => Word16 -> Word8 -> StateT (R2C02, a) m ()
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

writeControl :: (PBus m a) => Word8 -> StateT (R2C02, a) m ()
writeControl byte = do
    setReg PPUCTRL byte
    let nx = b0 byte
    let ny = b1 byte
    setTRAMBit L_NAMETABLE_X nx
    setTRAMBit L_NAMETABLE_Y ny
    return ()

writeMask :: (PBus m a) => Word8 -> StateT (R2C02, a) m ()
writeMask byte = setReg PPUMASK byte

writeScroll :: (PBus m a) => Word8 -> StateT (R2C02, a) m ()
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

writeAddress :: (PBus m a) => Word8 -> StateT (R2C02, a) m ()
writeAddress byte = do
    firstWrite <- not <$> getWriteToggle
    if firstWrite
        then do
            setWriteToggle True
            let fullbyte = fromIntegral byte :: Word16
            t <- getTRAM
            let t' = (shiftL (shiftTake 0 6 fullbyte) 8) .|. (t .&. 0x00FF)
            setTRAM t'
        else do
            setWriteToggle False
            let fullbyte = fromIntegral byte :: Word16
            t <- getTRAM
            let t' = (fullbyte) .|. (t .&. 0xFF00)
            setTRAM t'
            setVRAM t'

setDEBUG :: (PBus m a) => String -> StateT (R2C02, a) m ()
setDEBUG str = modifyFst (\ppu -> ppu{context = (context ppu){cdebug = str}})

writeData :: (PBus m a) => Word8 -> StateT (R2C02, a) m ()
writeData byte = do
    v <- getVRAM
    writeByte v byte
    increment_mode <- getCTRLFlag C_INCREMENT_MODE
    if increment_mode then setVRAM (v + 32) else setVRAM (v + 1)
    setDEBUG "After after after"

incCoarseX :: (PBus m a) => StateT (R2C02, a) m ()
incCoarseX = do
    coarseX <- getVRAMData L_COARSE_X
    if coarseX < 31
        then setVRAMData L_COARSE_X (coarseX + 1)
        else do
            setVRAMData L_COARSE_X 0
            ntx <- getVRAMBit L_NAMETABLE_X
            setVRAMBit L_NAMETABLE_X (not ntx)

incCoarseY :: (PBus m a) => StateT (R2C02, a) m ()
incCoarseY = do
    coarseY <- getVRAMData L_COARSE_Y
    if coarseY == 27
        then do
            setVRAMData L_COARSE_Y 0
            nty <- getVRAMBit L_NAMETABLE_Y
            setVRAMBit L_NAMETABLE_Y (not nty)
        else
            if coarseY == 31
                then do
                    setVRAMData L_COARSE_Y 0
                else do
                    setVRAMData L_COARSE_Y (coarseY + 1)

incFineY :: (PBus m a) => StateT (R2C02, a) m ()
incFineY = do
    fineY <- getVRAMData L_FINE_Y
    if fineY < 7
        then setVRAMData L_FINE_Y (fineY + 1)
        else do
            setVRAMData L_FINE_Y 0
            incCoarseY

nametableBase :: Mirroring -> Word16 -> Word16
nametableBase Horizontal addr
    | (addr >= 0x2000 && addr <= 0x23FF) = 0x000
    | (addr >= 0x2400 && addr <= 0x27FF) = 0x400
    | (addr >= 0x2800 && addr <= 0x2BFF) = 0x000
    | (addr >= 0x2C00 && addr <= 0x2FFF) = 0x400
    | otherwise = error "Address out of range"
nametableBase Vertical addr
    | (addr >= 0x2000 && addr <= 0x23FF) = 0x000
    | (addr >= 0x2400 && addr <= 0x27FF) = 0x000
    | (addr >= 0x2800 && addr <= 0x2BFF) = 0x400
    | (addr >= 0x2C00 && addr <= 0x2FFF) = 0x400
    | otherwise = error "Address out of range"


-- Tick

getCycle :: (PBus m a) => StateT (R2C02, a) m Int
getCycle = ppuCycle . context . fst <$> get

getScanline :: (PBus m a) => StateT (R2C02, a) m Int
getScanline = ppuScanline . context . fst <$> get

setCycle :: (PBus m a) => Int -> StateT (R2C02, a) m ()
setCycle c = modifyFst (\ppu -> ppu{context = (context ppu){ppuCycle = c}})

setScanline :: (PBus m a) => Int -> StateT (R2C02, a) m ()
setScanline s = modifyFst (\ppu -> ppu{context = (context ppu){ppuScanline = s}})

setComplete :: (PBus m a) => Bool -> StateT (R2C02, a) m ()
setComplete b = modifyFst (\ppu -> ppu{context = (context ppu){complete = b}})

incScanline :: (PBus m a) => StateT (R2C02, a) m ()
incScanline = do
    scanline <- getScanline
    if scanline >= 260
    then do
        setScanline (-1)
        setComplete True
    else do
        setScanline (scanline + 1)

incCycle :: (PBus m a) => StateT (R2C02, a) m ()
incCycle = do
    cycle <- getCycle
    if cycle >= 340 
    then do
        setCycle 0
        incScanline
    else do
        setCycle (cycle + 1)

handleVisibleScanline :: (PBus m a) => StateT (R2C02, a) m ()
handleVisibleScanline = return ()

setNMI :: (PBus m a) => Bool -> StateT (R2C02, a) m ()
setNMI b = modifyFst (\ppu -> ppu{context = (context ppu){ppuNMI = b}})

handleEndOfFrame :: (PBus m a) => StateT (R2C02, a) m ()
handleEndOfFrame = do
    scanline <- getScanline
    cycle <- getCycle
    enableNMI <- getCTRLFlag C_ENABLE_NMI
    when (scanline == 241 && cycle == 1) (do
        setSTATUSFlag S_VERTICAL_BLANK True
        when enableNMI (setNMI True)
        )

handleComposition :: (PBus m a) => StateT (R2C02, a) m ()
handleComposition = return ()

renderPixel :: (PBus m a) => StateT (R2C02, a) m ()
renderPixel = return ()

tickBackground :: (PBus m a) => StateT (R2C02, a) m ()
tickBackground = do
    scanline <- getScanline
    renderBackground <- getMASKFlag M_RENDER_BACKGROUND
    when (scanline >= (-1) && scanline < 240) handleVisibleScanline
    when (scanline >= 241  && scanline < 261) handleEndOfFrame
    when (renderBackground) handleComposition
    renderPixel
    incCycle

tick :: (PBus m a) => StateT (R2C02, a) m ()
tick = do
    tickBackground

