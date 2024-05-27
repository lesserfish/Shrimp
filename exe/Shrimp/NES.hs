{-# LANGUAGE MultiParamTypeClasses #-}
module Shrimp.NES where

import Control.Monad (when)
import Control.Monad.State
import Data.Bits
import Data.Word
import Shrimp.AbstractBus
import qualified Shrimp.Cartridge as Cart
import qualified Shrimp.MOS6502 as CPU
import qualified Shrimp.Memory as Memory
import qualified Shrimp.R2C02 as PPU

data NESContext = NESContext

data NES = NES
    { cpu :: !CPU.MOS6502
    , ppu :: !PPU.R2C02
    , cartridge :: !Cart.CartridgeIO
    , cpuRAM :: !Memory.IORAM
    , nametableRAM :: !Memory.IORAM
    , paletteRAM :: !Memory.IORAM
    , context :: !NESContext
    , nClock :: !Int
    }

-- Helper functions
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

run :: (Monad m) => StateT a m b -> a -> m (a, b)
run s i = swap <$> runStateT s i

exec :: (Monad m) => StateT a m b -> a -> m a
exec = execStateT

-- CPU Interface

cpuReadRAM :: Word16 -> StateT NES IO Word8
cpuReadRAM addr = do
    nes <- get
    let real_addr = addr .&. 0x07FF -- Addresses 0x000 to 0x07FF is mirrored through $1FFFF
    byte <- liftIO $ Memory.readByteIO (cpuRAM nes) real_addr
    return byte

cpuReadPPU :: Word16 -> StateT NES IO Word8
cpuReadPPU addr = do
    nes <- get
    let real_addr = addr .&. 0x0007 -- Addresses 0x2000 to 0x2007 is mirrored through $3FFF
    (byte, out) <- liftIO $ runStateT (PPU.cpuRead real_addr) (ppu nes, nes)
    let nes' = flattenPPU out
    return byte

cpuReadAPU :: Word16 -> StateT NES IO Word8
cpuReadAPU addr = return 0 -- TODO: Implement APU Support

cpuReadControl :: Word16 -> StateT NES IO Word8
cpuReadControl addr = return 0 -- TODO: Implement Control Support

cpuReadCart :: Word16 -> StateT NES IO Word8
cpuReadCart addr = do
    nes <- get
    (cart', byte) <- liftIO $ Cart.cpuReadIO (cartridge nes) addr -- The mapper fixes the address by itself
    put nes{cartridge = cart'}
    return byte

cpuWriteRAM :: Word16 -> Word8 -> StateT NES IO ()
cpuWriteRAM addr byte = do
    nes <- get
    let real_addr = addr .&. 0x07FF -- Addresses 0x000 to 0x07FF is mirrored through $1FFFF
    liftIO $ Memory.writeByteIO (cpuRAM nes) real_addr byte

cpuWritePPU :: Word16 -> Word8 -> StateT NES IO ()
cpuWritePPU addr byte = do
    nes <- get
    let real_addr = addr .&. 0x0007 -- Addresses 0x2000 to 0x2007 is mirrored through $3FFF
    out <- liftIO $ execStateT (PPU.cpuWrite real_addr byte) (ppu nes, nes)
    let nes' = flattenPPU out
    put nes'

cpuWriteAPU :: Word16 -> Word8 -> StateT NES IO ()
cpuWriteAPU addr byte = return () -- TODO: Implement APU Support

cpuWriteControl :: Word16 -> Word8 -> StateT NES IO ()
cpuWriteControl addr byte = return () -- TODO: Implement Control Support

cpuWriteCart :: Word16 -> Word8 -> StateT NES IO ()
cpuWriteCart addr byte = do
    nes <- get
    cart' <- liftIO $ Cart.cpuWriteIO (cartridge nes) addr byte -- The mapper fixes the address by itself
    put nes{cartridge = cart'}


instance CBus IO NES where
    cReadByte addr nes
        | (addr >= 0x0000 && addr <= 0x1FFF) = run (cpuReadRAM addr) nes
        | (addr >= 0x2000 && addr <= 0x3FFF) = run (cpuReadPPU addr) nes
        | (addr >= 0x4000 && addr <= 0x4015) = run (cpuReadAPU addr) nes
        | (addr >= 0x4016 && addr <= 0x4017) = run (cpuReadControl addr) nes
        | (addr >= 0x4020 && addr <= 0xFFFF) = run (cpuReadCart addr) nes
        | otherwise = return (nes, 0) -- TODO: Log error ?
    cWriteByte addr byte nes
        | (addr >= 0x0000 && addr <= 0x1FFF) = exec (cpuWriteRAM addr byte) nes
        | (addr >= 0x2000 && addr <= 0x3FFF) = exec (cpuWritePPU addr byte) nes
        | (addr >= 0x4000 && addr <= 0x4015) = exec (cpuWriteAPU addr byte) nes
        | (addr >= 0x4016 && addr <= 0x4017) = exec (cpuWriteControl addr byte) nes
        | (addr >= 0x4020 && addr <= 0xFFFF) = exec (cpuWriteCart addr byte) nes
        | otherwise = return nes -- TODO: Log error ?
    cPeek addr nes = snd <$> cReadByte addr nes
    cDebug log nes = do
        liftIO $ putStrLn log
        return nes

-- PPU Interface 

ppuReadPT :: Word16 -> StateT NES IO Word8
ppuReadPT addr = do
    nes <- get
    let cart = cartridge nes
    (cart', byte) <- liftIO $ Cart.ppuReadIO cart addr
    put nes{cartridge = cart'}
    return byte

ppuReadNT :: Word16 -> StateT NES IO Word8
ppuReadNT addr = do
    nes <- get
    let mirroring = Cart.hMirroring . Cart.cHeader . Cart.cartData . cartridge $ nes
    let baseaddr = PPU.nametableBase mirroring addr
    let addr' = baseaddr + (addr .&. 0x03FF)
    byte <- liftIO $ Memory.readByteIO (nametableRAM nes) addr'
    return byte

ppuReadPL :: Word16 -> StateT NES IO Word8
ppuReadPL addr = return 0 -- TODO: Implement Palette RAM support


ppuWritePT :: Word16 -> Word8 -> StateT NES IO ()
ppuWritePT addr byte = do
    nes <- get
    let cart = cartridge nes
    cart' <- liftIO $ Cart.ppuWriteIO cart addr byte
    put nes{cartridge = cart'}

ppuWriteNT :: Word16 -> Word8 -> StateT NES IO ()
ppuWriteNT addr byte = do
    nes <- get
    let mirroring = Cart.hMirroring . Cart.cHeader . Cart.cartData . cartridge $ nes
    let baseaddr = PPU.nametableBase mirroring addr
    let addr' = baseaddr + (addr .&. 0x03FF)
    liftIO $ Memory.writeByteIO (nametableRAM nes) addr' byte

ppuWritePL :: Word16 -> Word8 -> StateT NES IO ()
ppuWritePL addr byte = return () -- TODO: Implement Palette RAM support

instance PBus IO NES where
    pReadByte addr nes
        | (addr >= 0x0000 && addr <= 0x1FFF) = run (ppuReadPT addr) nes
        | (addr >= 0x2000 && addr <= 0x3EFF) = run (ppuReadNT addr) nes
        | (addr >= 0x3F00 && addr <= 0x3FFF) = run (ppuReadPL addr) nes
        | otherwise = return (nes, 0) -- TODO: Log error
    pWriteByte addr byte nes
        | (addr >= 0x0000 && addr <= 0x1FFF) = exec (ppuWritePT addr byte) nes
        | (addr >= 0x2000 && addr <= 0x3EFF) = exec (ppuWriteNT addr byte) nes
        | (addr >= 0x3F00 && addr <= 0x3FFF) = exec (ppuWritePL addr byte) nes
        | otherwise = return nes -- TODO: Log error
    pPeek addr nes = snd <$> pReadByte addr nes
    pSetPixel = undefined
    pDebug log nes = do
        liftIO $ putStrLn log
        return nes


flattenCPU :: (CPU.MOS6502, NES) -> NES
flattenCPU (mos6502, nes) = nes{cpu = mos6502}

flattenPPU :: (PPU.R2C02, NES) -> NES
flattenPPU (r2c02, nes) = nes{ppu = r2c02}

updateClock :: Int -> StateT NES IO ()
updateClock offset = do
    nes <- get
    put nes{nClock = (nClock nes) + offset}


tickCPU :: StateT NES IO ()
tickCPU = do
    nes <- get
    if (nClock nes) `mod` 3 == 0 -- The CPU clock is ~ 3x slowers than the PPU clock
        then do
            let mos6502 = cpu nes
            out <- liftIO $ execStateT CPU.tick (mos6502, nes)
            let nes' = flattenCPU out
            put nes'
        else put nes

tickPPU :: StateT NES IO ()
tickPPU = do
    nes <- get
    let r2c02 = ppu nes
    out <- liftIO $ execStateT PPU.tick (r2c02, nes)
    let nes' = flattenPPU out
    put nes'

tick :: StateT NES IO ()
tick = do
    nes <- get
    nes <- get
    tickCPU
    tickPPU
    updateClock 1

reset :: StateT NES IO ()
reset = do
    nes <- get
    (cpu', _) <- liftIO $ execStateT CPU.reset (cpu nes, nes)
    (ppu', _) <- liftIO $ execStateT PPU.reset (ppu nes, nes)
    --cart' <- liftIO $ Cart.reset $ cartridge nes -- TODO: Implement cartridge resetting. Probably just run a fromCartDataIO.
    liftIO $ Memory.resetIO (cpuRAM nes)
    liftIO $ Memory.resetIO (nametableRAM nes)
    liftIO $ Memory.resetIO (paletteRAM nes)
    let nes' =
            nes
                { cpu = cpu'
                , ppu = ppu'
                --, cartridge = cart'
                , context = NESContext
                , nClock = 0
                } 
    put nes'

empty :: IO NES
empty = do
    nocart <- Cart.emptyCartridge
    cpuram <- Memory.newIO 0x800 0
    nmtbram <- Memory.newIO 0x800 0
    pltram <- Memory.newIO 0x1F 0
    return $ NES
        { cpu = CPU.mos6502
        , ppu = PPU.r2c02
        , cartridge = nocart
        , cpuRAM = cpuram
        , nametableRAM = nmtbram
        , paletteRAM = pltram
        , context = NESContext
        , nClock = 0
        }

loadNES :: FilePath -> IO NES
loadNES fp = do
    cart <- Cart.loadCartridgeIO fp
    emptynes <- empty
    let nes' = emptynes{cartridge = cart}
    nes <- execStateT reset nes'
    return nes

setCPUComplete :: Bool -> NES -> NES
setCPUComplete b nes = nes' where
    mos = cpu nes
    ctx = CPU.context mos
    ctx' = ctx{CPU.complete = b}
    mos' = mos{CPU.context = ctx'}
    nes' = nes{cpu = mos'}

setPPUComplete :: Bool -> NES -> NES
setPPUComplete b nes = nes' where
    r2 = ppu nes
    ctx = PPU.context r2
    ctx' = ctx{PPU.complete = b}
    r2' = r2{PPU.context = ctx'}
    nes' = nes{ppu = r2'}

