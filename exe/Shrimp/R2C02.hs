module Shrimp.R2C02 (
    R2C02 (..),
    r2c02,
    tick,
    reset,
    cpuRead,
    cpuWrite,
) where

import Control.Monad.State
import Data.Bits
import Data.Word
import Shrimp.AbstractBus
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
    }

data RegisterValue a = RegisterValue (Registers -> a)

class RegisterType a where
    readRegister :: REGISTER -> RegisterValue a
    writeRegister :: REGISTER -> a -> Registers -> Registers

instance RegisterType Word8 where
    readRegister PPUCTRL = RegisterValue ppuctrl
    readRegister PPUMASK = RegisterValue ppumask
    readRegister PPUSTATUS = RegisterValue ppustatus
    readRegister FINEX = RegisterValue fineX
    readRegister _ = error "Attempted to read Word16 as Word8"

    writeRegister PPUCTRL val regs = regs{ppuctrl = val}
    writeRegister PPUMASK val regs = regs{ppumask = val}
    writeRegister PPUSTATUS val regs = regs{ppustatus = val}
    writeRegister FINEX val regs = regs{fineX = val}
    writeRegister _ _ _ = error "Attempted to write Word16 to Word8"

instance RegisterType Word16 where
    readRegister VRAM = RegisterValue vram
    readRegister TRAM = RegisterValue tram
    readRegister _ = error "Attempted to read Word8 as Word16"

    writeRegister VRAM val regs = regs{vram = val}
    writeRegister TRAM val regs = regs{tram = val}
    writeRegister _ _ _ = error "Attempted to write Word8 to Word16"

mapReg :: (PBus m a, RegisterType b) => REGISTER -> (b -> b) -> StateT (R2C02, a) m ()
mapReg reg func = do
    (ppu, bus) <- get
    let registers = ppuRegisters ppu
        RegisterValue getter = readRegister reg
        currentVal = getter registers
        updatedVal = func currentVal
        registers' = writeRegister reg updatedVal registers
        ppu' = ppu{ppuRegisters = registers'}
    put (ppu', bus)

setReg :: (PBus m a, RegisterType b) => REGISTER -> b -> StateT (R2C02, a) m ()
setReg reg value = mapReg reg (\_ -> value)

getReg :: (PBus m a, RegisterType b) => REGISTER -> StateT (R2C02, a) m b
getReg reg = do
    (ppu, _) <- get
    let registers = ppuRegisters ppu
    let RegisterValue getter = readRegister reg
    let regval = getter registers
    return regval

getCTRLFlag :: (PBus m a) => CTRLFLAG -> StateT (R2C02, a) m Bool
getCTRLFlag C_NAMETABLE_X = fmap b0 (getReg PPUCTRL :: (PBus m a) => StateT (R2C02, a) m Word8)
getCTRLFlag C_NAMETABLE_Y = fmap b1 (getReg PPUCTRL :: (PBus m a) => StateT (R2C02, a) m Word8)
getCTRLFlag C_INCREMENT_MODE = fmap b2 (getReg PPUCTRL :: (PBus m a) => StateT (R2C02, a) m Word8)
getCTRLFlag C_PATTERN_SPRITE = fmap b3 (getReg PPUCTRL :: (PBus m a) => StateT (R2C02, a) m Word8)
getCTRLFlag C_PATTERN_BACKGROUND = fmap b4 (getReg PPUCTRL :: (PBus m a) => StateT (R2C02, a) m Word8)
getCTRLFlag C_SPRITE_SIZE = fmap b5 (getReg PPUCTRL :: (PBus m a) => StateT (R2C02, a) m Word8)
getCTRLFlag C_SLAVE_MDOE = fmap b6 (getReg PPUCTRL :: (PBus m a) => StateT (R2C02, a) m Word8)
getCTRLFlag C_ENABLE_NMI = fmap b7 (getReg PPUCTRL :: (PBus m a) => StateT (R2C02, a) m Word8)

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
getMASKFlag M_GRAYSCALE = fmap b0 (getReg PPUMASK :: (PBus m a) => StateT (R2C02, a) m Word8)
getMASKFlag M_RENDER_BACKGROUND_LEFT = fmap b1 (getReg PPUMASK :: (PBus m a) => StateT (R2C02, a) m Word8)
getMASKFlag M_RENDER_SPRITES_LEFT = fmap b2 (getReg PPUMASK :: (PBus m a) => StateT (R2C02, a) m Word8)
getMASKFlag M_RENDER_BACKGROUND = fmap b3 (getReg PPUMASK :: (PBus m a) => StateT (R2C02, a) m Word8)
getMASKFlag M_RENDER_SPRITES = fmap b4 (getReg PPUMASK :: (PBus m a) => StateT (R2C02, a) m Word8)
getMASKFlag M_ENHANCE_RED = fmap b5 (getReg PPUMASK :: (PBus m a) => StateT (R2C02, a) m Word8)
getMASKFlag M_ENHANCE_GREEN = fmap b6 (getReg PPUMASK :: (PBus m a) => StateT (R2C02, a) m Word8)
getMASKFlag M_ENHANCE_BLUE = fmap b7 (getReg PPUMASK :: (PBus m a) => StateT (R2C02, a) m Word8)

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
getSTATUSFlag S_SPRITE_OVERFLOW = fmap b5 (getReg PPUSTATUS :: (PBus m a) => StateT (R2C02, a) m Word8)
getSTATUSFlag S_SPRITE_ZERO_HIT = fmap b6 (getReg PPUSTATUS :: (PBus m a) => StateT (R2C02, a) m Word8)
getSTATUSFlag S_VERTICAL_BLANK = fmap b7 (getReg PPUSTATUS :: (PBus m a) => StateT (R2C02, a) m Word8)

setSTATUSFlag :: (PBus m a) => STATUSFLAG -> Bool -> StateT (R2C02, a) m ()
setSTATUSFlag S_SPRITE_OVERFLOW v = mapReg PPUSTATUS (\ctrl -> if v then (setBit ctrl 5) :: Word8 else (clearBit ctrl 5) :: Word8)
setSTATUSFlag S_SPRITE_ZERO_HIT v = mapReg PPUSTATUS (\ctrl -> if v then (setBit ctrl 6) :: Word8 else (clearBit ctrl 6) :: Word8)
setSTATUSFlag S_VERTICAL_BLANK v = mapReg PPUSTATUS (\ctrl -> if v then (setBit ctrl 7) :: Word8 else (clearBit ctrl 7) :: Word8)

getTRAMData :: (PBus m a) => LOOPYFLAG -> StateT (R2C02, a) m Word8
getTRAMData L_COARSE_X = do
    loopy <- getReg TRAM :: (PBus m a) => StateT (R2C02, a) m Word16
    let bits = fromIntegral . (.&. 0x1F) $ loopy :: Word8
    return bits
getTRAMData L_COARSE_Y = do
    loopy <- getReg TRAM :: (PBus m a) => StateT (R2C02, a) m Word16
    let bits = fromIntegral . (.&. 0x1F) $ shiftR loopy 5 :: Word8
    return bits
getTRAMData L_FINE_Y = do
    loopy <- getReg TRAM :: (PBus m a) => StateT (R2C02, a) m Word16
    let bits = fromIntegral . (.&. 0x7) $ shiftR loopy 12 :: Word8
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
    let bits = fromIntegral . (.&. 0x1F) $ loopy :: Word8
    return bits
getVRAMData L_COARSE_Y = do
    loopy <- getReg VRAM :: (PBus m a) => StateT (R2C02, a) m Word16
    let bits = fromIntegral . (.&. 0x1F) $ shiftR loopy 5 :: Word8
    return bits
getVRAMData L_FINE_Y = do
    loopy <- getReg VRAM :: (PBus m a) => StateT (R2C02, a) m Word16
    let bits = fromIntegral . (.&. 0x7) $ shiftR loopy 12 :: Word8
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
    loopy <- getReg TRAM :: (PBus m a) => StateT (R2C02, a) m Word16
    let loopy' = (loopy .&. 0x47FF) + shiftL ((fromIntegral val) .&. 0x7) 12
    setReg TRAM loopy' :: (PBus m a) => StateT (R2C02, a) m ()
setVRAMData _ _ = error "Incorrect Flag"

getTRAMBit :: (PBus m a) => LOOPYFLAG -> StateT (R2C02, a) m Bool
getTRAMBit L_NAMETABLE_X = fmap b10 (getReg TRAM :: (PBus m a) => StateT (R2C02, a) m Word16)
getTRAMBit L_NAMETABLE_Y = fmap b11 (getReg TRAM :: (PBus m a) => StateT (R2C02, a) m Word16)
getTRAMBit _ = error "Incorrect Flag"

setTRAMBit :: (PBus m a) => LOOPYFLAG -> Bool -> StateT (R2C02, a) m ()
setTRAMBit L_NAMETABLE_X v = mapReg TRAM (\tram -> if v then (setBit tram 10) :: Word16 else (clearBit tram 10) :: Word16)
setTRAMBit L_NAMETABLE_Y v = mapReg TRAM (\tram -> if v then (setBit tram 11) :: Word16 else (clearBit tram 11) :: Word16)
setTRAMBit _ _ = error "Incorrect Flag"

getVRAMBit :: (PBus m a) => LOOPYFLAG -> StateT (R2C02, a) m Bool
getVRAMBit L_NAMETABLE_X = fmap b10 (getReg VRAM :: (PBus m a) => StateT (R2C02, a) m Word16)
getVRAMBit L_NAMETABLE_Y = fmap b11 (getReg VRAM :: (PBus m a) => StateT (R2C02, a) m Word16)
getVRAMBit _ = error "Incorrect Flag"

setVRAMBit :: (PBus m a) => LOOPYFLAG -> Bool -> StateT (R2C02, a) m ()
setVRAMBit L_NAMETABLE_X v = mapReg VRAM (\tram -> if v then (setBit tram 10) :: Word16 else (clearBit tram 10) :: Word16)
setVRAMBit L_NAMETABLE_Y v = mapReg VRAM (\tram -> if v then (setBit tram 11) :: Word16 else (clearBit tram 11) :: Word16)
setVRAMBit _ _ = error "Incorrect Flag"

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
    { writeToggle :: Bool
    }

-- R2C02
data R2C02 = R2C02
    { ppuRegisters :: Registers
    , ppuContext :: Context
    }

-- CPU Interface
cpuRead :: (PBus m a) => Word16 -> StateT (R2C02, a) m Word8
cpuRead addr
    | addr == 0x0000 = return 0 -- Control
    | addr == 0x0001 = return 0 -- Mask
    | addr == 0x0002 = return 0 -- Status
    | addr == 0x0003 = return 0 -- OAM Address
    | addr == 0x0004 = return 0 -- OAM Data
    | addr == 0x0005 = return 0 -- Scroll
    | addr == 0x0006 = return 0 -- Address
    | addr == 0x0007 = return 0 -- Data
    | otherwise = return 0 --

cpuWrite :: (PBus m a) => Word16 -> Word8 -> StateT (R2C02, a) m ()
cpuWrite addr byte
    | addr == 0x0000 = return ()
    | addr == 0x0001 = return ()
    | addr == 0x0002 = return () -- Status: Read Only
    | addr == 0x0003 = return () -- TODO: Implement OAS ADDR support
    | addr == 0x0004 = return () -- TODO: Implement OAS DATA support
    | addr == 0x0005 = return ()
    | addr == 0x0006 = return ()
    | addr == 0x0007 = return ()
    | otherwise = return () -- TODO: Log error

writeControl :: (PBus m a) => Word8 -> StateT (R2C02, a) m ()
writeControl byte = do
    setReg PPUCTRL byte
    let nx = b0 byte
    let ny = b1 byte
    setTRAMBit L_NAMETABLE_X nx
    setTRAMBit L_NAMETABLE_Y ny
    return ()

-- Public Methods
--
reset :: (PBus m a) => StateT (R2C02, a) m ()
reset = do
    (r2c02, bus) <- get
    let r2c02' = r2c02
    put (r2c02', bus)

tick :: (PBus m a) => StateT (R2C02, a) m ()
tick = do
    return ()

r2c02 :: R2C02
r2c02 =
    R2C02
        {
        }
