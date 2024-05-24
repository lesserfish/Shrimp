module Shrimp.NES where

import Control.Monad.State
import Data.Bits
import Data.Word
import Shrimp.AbstractBus
import qualified Shrimp.Cartridge as Cart
import qualified Shrimp.INES as INES
import qualified Shrimp.IO as IO
import qualified Shrimp.MOS6502 as CPU
import qualified Shrimp.Memory as Memory
import qualified Shrimp.R2C02 as PPU

data NESContext = NESContext

data NES = NES
    { cpu :: CPU.MOS6502
    , ppu :: PPU.R2C02
    , cartridge :: Cart.Cartridge
    , cpuRAM :: Memory.RAM
    , nametableRAM :: Memory.RAM
    , paletteRAM :: Memory.RAM
    , context :: NESContext
    , nClock :: !Int
    }

-- Helper functions
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

run :: State a b -> a -> (a, b)
run = (swap .) . runState

exec :: State a b -> a -> a
exec = execState

flattenCPU :: (CPU.MOS6502, NES) -> NES
flattenCPU (mos6502, nes) = nes{cpu = mos6502}

flattenPPU :: (PPU.R2C02, NES) -> NES
flattenPPU (r2c02, nes) = nes{ppu = r2c02}

-- CPU Read interface
-- RAM
cpuReadRAM :: Word16 -> State NES Word8
cpuReadRAM addr = do
    nes <- get
    let real_addr = addr .&. 0x07FF -- Addresses 0x000 to 0x07FF is mirrored through $1FFFF
    let byte = Memory.readByte (cpuRAM nes) real_addr
    return byte

-- PPU
cpuReadPPU :: Word16 -> State NES Word8
cpuReadPPU addr = do
    nes <- get
    let real_addr = addr .&. 0x0007 -- Addresses 0x2000 to 0x2007 is mirrored through $3FFF
    let (byte, out) = runState (PPU.cpuRead real_addr) (ppu nes, nes)
    let nes' = flattenPPU out
    put nes'
    return byte

-- APU
cpuReadAPU :: Word16 -> State NES Word8
cpuReadAPU addr = return 0 -- TODO: Implement APU Support

-- Controls
cpuReadControl :: Word16 -> State NES Word8
cpuReadControl addr = return 0 -- TODO: Implement Control Support

-- Cartridge
cpuReadCart :: Word16 -> State NES Word8
cpuReadCart addr = do
    nes <- get
    let (cart', byte) = Cart.cpuRead (cartridge nes) addr -- The mapper fixes the address by itself
    let nes' = nes{cartridge = cart'}
    put nes'
    return byte

-- CPU Write interface
-- RAM
cpuWriteRAM :: Word16 -> Word8 -> State NES ()
cpuWriteRAM addr byte = do
    nes <- get
    let real_addr = addr .&. 0x07FF -- Addresses 0x000 to 0x07FF is mirrored through $1FFFF
    let ram' = Memory.writeByte (cpuRAM nes) real_addr byte
    let nes' = nes{cpuRAM = ram'}
    put nes'

-- PPU
cpuWritePPU :: Word16 -> Word8 -> State NES ()
cpuWritePPU addr byte = do
    nes <- get
    let real_addr = addr .&. 0x0007 -- Addresses 0x2000 to 0x2007 is mirrored through $3FFF
    let out = execState (PPU.cpuWrite real_addr byte) (ppu nes, nes)
    let nes' = flattenPPU out
    put nes'

-- APU
cpuWriteAPU :: Word16 -> Word8 -> State NES ()
cpuWriteAPU addr byte = return () -- TODO: Implement APU Support

-- Controls
cpuWriteControl :: Word16 -> Word8 -> State NES ()
cpuWriteControl addr byte = return () -- TODO: Implement Control Support

-- Cartridge
cpuWriteCart :: Word16 -> Word8 -> State NES ()
cpuWriteCart addr byte = do
    nes <- get
    let cart' = Cart.cpuWrite (cartridge nes) addr byte -- The mapper fixes the address by itself
    let nes' = nes{cartridge = cart'}
    put nes'

instance PCBus NES where
    pcReadByte addr nes
        | (addr >= 0x0000 && addr <= 0x1FFF) = run (cpuReadRAM addr) nes
        | (addr >= 0x2000 && addr <= 0x3FFF) = run (cpuReadPPU addr) nes
        | (addr >= 0x4000 && addr <= 0x4015) = run (cpuReadAPU addr) nes
        | (addr >= 0x4016 && addr <= 0x4017) = run (cpuReadControl addr) nes
        | (addr >= 0x4020 && addr <= 0xFFFF) = run (cpuReadCart addr) nes
        | otherwise = (nes, 0) -- TODO: Log error
    pcWriteByte addr byte nes
        | (addr >= 0x0000 && addr <= 0x1FFF) = exec (cpuWriteRAM addr byte) nes
        | (addr >= 0x2000 && addr <= 0x3FFF) = exec (cpuWritePPU addr byte) nes
        | (addr >= 0x4000 && addr <= 0x4015) = exec (cpuWriteAPU addr byte) nes
        | (addr >= 0x4016 && addr <= 0x4017) = exec (cpuWriteControl addr byte) nes
        | (addr >= 0x4020 && addr <= 0xFFFF) = exec (cpuWriteCart addr byte) nes
        | otherwise = nes -- TODO: Log error
    pcPeek addr nes
        | (addr >= 0x0000 && addr <= 0x1FFF) = snd $ run (cpuReadRAM addr) nes
        | (addr >= 0x2000 && addr <= 0x3FFF) = snd $ run (cpuReadPPU addr) nes
        | (addr >= 0x4000 && addr <= 0x4015) = snd $ run (cpuReadAPU addr) nes
        | (addr >= 0x4016 && addr <= 0x4017) = snd $ run (cpuReadControl addr) nes
        | (addr >= 0x4020 && addr <= 0xFFFF) = snd $ run (cpuReadCart addr) nes
        | otherwise = 0 -- TODO: Log error

-- PPU Interface

ppuReadPT :: Word16 -> State NES Word8
ppuReadPT addr = do
    nes <- get
    let cart = cartridge nes
    let (cart', byte) = Cart.ppuRead cart addr
    put nes{cartridge = cart'}
    return byte

ppuReadNT :: Word16 -> State NES Word8
ppuReadNT addr = do
    nes <- get
    let mirroring = INES.hMirroring . Cart.cHeader . cartridge $ nes
    let baseaddr = PPU.nametableBase mirroring addr
    let addr' = baseaddr + (addr .&. 0x03FF)
    let byte = Memory.readByte (nametableRAM nes) addr'
    return byte

ppuReadPL :: Word16 -> State NES Word8
ppuReadPL addr = return 0 -- TODO: Implement Palette RAM support

ppuWritePT :: Word16 -> Word8 -> State NES ()
ppuWritePT addr byte = do
    nes <- get
    let cart = cartridge nes
    let cart' = Cart.ppuWrite cart addr byte
    put nes{cartridge = cart'}

ppuWriteNT :: Word16 -> Word8 -> State NES ()
ppuWriteNT addr byte = do
    nes <- get
    let mirroring = INES.hMirroring . Cart.cHeader . cartridge $ nes
    let baseaddr = PPU.nametableBase mirroring addr
    let addr' = baseaddr + (addr .&. 0x03FF)
    let ram' = Memory.writeByte (nametableRAM nes) addr' byte
    put nes{nametableRAM = ram'}

ppuWritePL :: Word16 -> Word8 -> State NES ()
ppuWritePL addr byte = return () -- TODO: Implement Palette RAM support

instance PPBus NES where
    ppReadByte addr nes
        | (addr >= 0x0000 && addr <= 0x1FFF) = run (ppuReadPT addr) nes
        | (addr >= 0x2000 && addr <= 0x3EFF) = run (ppuReadNT addr) nes
        | (addr >= 0x3F00 && addr <= 0x3FFF) = run (ppuReadPL addr) nes
    ppWriteByte addr byte nes
        | (addr >= 0x0000 && addr <= 0x1FFF) = exec (ppuWritePT addr byte) nes
        | (addr >= 0x2000 && addr <= 0x3EFF) = exec (ppuWriteNT addr byte) nes
        | (addr >= 0x3F00 && addr <= 0x3FFF) = exec (ppuWritePL addr byte) nes
    ppPeek addr nes
        | (addr >= 0x0000 && addr <= 0x1FFF) = snd $ run (ppuReadPT addr) nes
        | (addr >= 0x2000 && addr <= 0x3EFF) = snd $ run (ppuReadNT addr) nes
        | (addr >= 0x3F00 && addr <= 0x3FFF) = snd $ run (ppuReadPL addr) nes
    ppSetPixel (x, y) value nes = nes -- TODO: Implement SetPixel support
    ppEmitNMI nes = nes -- TODO: Implement NMI support

ppuWrite :: Word16 -> Word8 -> State NES ()
ppuWrite addr byte
    | (addr >= 0x0000 && addr <= 0x1FFF) = ppuWritePT addr byte
    | (addr >= 0x2000 && addr <= 0x3EFF) = ppuWriteNT addr byte
    | (addr >= 0x3F00 && addr <= 0x3FFF) = ppuWritePL addr byte

updateClock :: Int -> State NES ()
updateClock offset = do
    nes <- get
    let nes' = nes{nClock = (nClock nes) + offset}
    put nes'

tickCPU :: State NES ()
tickCPU = do
    nes <- get
    if (nClock nes) `mod` 3 == 0 -- The CPU clock is ~ 3x slowers than the PPU clock
        then do
            let mos6502 = cpu nes
            let nes' = flattenCPU $ execState CPU.tick (mos6502, nes)
            put nes'
        else put nes

tickPPU :: State NES ()
tickPPU = do
    nes <- get
    let r2c02 = ppu nes
    let nes' = flattenPPU $ execState PPU.tick (r2c02, nes)
    put nes'

tick :: State NES ()
tick = do
    tickCPU
    tickPPU
    updateClock 1

reset :: State NES ()
reset = do
    nes <- get
    let (cpu', _) = exec CPU.reset (cpu nes, nes) -- CPU Reset does not change the NES
    let cart' = Cart.reset $ cartridge nes
    let (ppu', _) = exec PPU.reset (ppu nes, nes) -- PPU Reset does not change the NES
    let nes' =
            nes
                { cpu = cpu'
                , ppu = ppu'
                , cpuRAM = Memory.new 0x800 0
                , nametableRAM = Memory.new 0x800 0
                , paletteRAM = Memory.new 0x1F 0
                , context = NESContext
                , nClock = 0
                } -- TODO: Do you need to reset the cartridge as well???
    put nes'

empty :: NES
empty =
    NES
        { cpu = CPU.mos6502
        , ppu = PPU.r2c02
        , cartridge = Cart.emptyCartridge
        , cpuRAM = Memory.new 0x800 0
        , nametableRAM = Memory.new 0x800 0
        , paletteRAM = Memory.new 0x1F 0
        , context = NESContext
        , nClock = 0
        }

loadNES :: FilePath -> IO NES
loadNES fp = do
    cart <- Cart.loadCartridge fp
    let nes' = empty{cartridge = cart}
    let nes = execState reset nes'
    return nes
