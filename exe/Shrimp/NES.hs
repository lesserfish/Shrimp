{-# LANGUAGE FlexibleInstances #-}
module Shrimp.NES where

import Control.Monad (when)
import Control.Monad.State
import Data.Bits
import Data.Word
import Shrimp.AbstractBus
import qualified Shrimp.Cartridge as Cart
import qualified Shrimp.IO as IO
import qualified Shrimp.MOS6502 as CPU
import qualified Shrimp.Memory as Memory
import qualified Shrimp.R2C02 as PPU
import Shrimp.MOS6502 (disassembleL)

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

-- CPU Interface

cpuReadRAM :: Word16 -> StateT NES IO Word8
cpuReadRAM addr = do
    liftIO . putStrLn $ "CPU read RAM"
    nes <- get
    let real_addr = addr .&. 0x07FF -- Addresses 0x000 to 0x07FF is mirrored through $1FFFF
    byte <- liftIO $ Memory.readByteIO (cpuRAM nes) real_addr

    (ppu', _) <- PPU.mDispatcher (PPU.setDEBUG "Debug from CPU read RAM")(ppu nes)
    put nes{ppu = ppu'}

    return byte

cpuReadPPU :: Word16 -> StateT NES IO Word8
cpuReadPPU addr = do
    nes <- get
    let real_addr = addr .&. 0x0007 -- Addresses 0x2000 to 0x2007 is mirrored through $3FFF
    (ppu', byte) <- PPU.mDispatcher (PPU.cpuRead real_addr) (ppu nes)
    put nes{ppu = ppu'}
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
    (ppu', _) <- PPU.mDispatcher (PPU.cpuWrite real_addr byte) (ppu nes)
    put nes{ppu = ppu'}

cpuWriteAPU :: Word16 -> Word8 -> StateT NES IO ()
cpuWriteAPU addr byte = return () -- TODO: Implement APU Support

cpuWriteControl :: Word16 -> Word8 -> StateT NES IO ()
cpuWriteControl addr byte = return () -- TODO: Implement Control Support

cpuWriteCart :: Word16 -> Word8 -> StateT NES IO ()
cpuWriteCart addr byte = do
    nes <- get
    cart' <- liftIO $ Cart.cpuWriteIO (cartridge nes) addr byte -- The mapper fixes the address by itself
    put nes{cartridge = cart'}


instance MCBus (StateT NES IO) where
    mcReadByte addr
        | (addr >= 0x0000 && addr <= 0x1FFF) = cpuReadRAM addr
        | (addr >= 0x2000 && addr <= 0x3FFF) = cpuReadPPU addr
        | (addr >= 0x4000 && addr <= 0x4015) = cpuReadAPU addr
        | (addr >= 0x4016 && addr <= 0x4017) = cpuReadControl addr
        | (addr >= 0x4020 && addr <= 0xFFFF) = cpuReadCart addr
        | otherwise = return 0
    mcWriteByte addr byte
        | (addr >= 0x0000 && addr <= 0x1FFF) = cpuWriteRAM addr byte
        | (addr >= 0x2000 && addr <= 0x3FFF) = cpuWritePPU addr byte
        | (addr >= 0x4000 && addr <= 0x4015) = cpuWriteAPU addr byte
        | (addr >= 0x4016 && addr <= 0x4017) = cpuWriteControl addr byte
        | (addr >= 0x4020 && addr <= 0xFFFF) = cpuWriteCart addr byte
        | otherwise = return ()
    mcPeek addr = do
        nes <- get
        byte <- mcReadByte addr
        put nes -- Little trickery. Unsure if this works. I think so.
        return byte
    mcDebug log = do
        liftIO $ putStrLn log

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

instance MPBus (StateT NES IO) where
    mpReadByte addr 
        | (addr >= 0x0000 && addr <= 0x1FFF) = ppuReadPT addr
        | (addr >= 0x2000 && addr <= 0x3EFF) = ppuReadNT addr
        | (addr >= 0x3F00 && addr <= 0x3FFF) = ppuReadPL addr
    mpWriteByte addr byte
        | (addr >= 0x0000 && addr <= 0x1FFF) = ppuWritePT addr byte
        | (addr >= 0x2000 && addr <= 0x3EFF) = ppuWriteNT addr byte
        | (addr >= 0x3F00 && addr <= 0x3FFF) = ppuWritePL addr byte
    mpPeek addr = do
       nes <- get 
       byte <- mpReadByte addr
       put nes
       return byte
    mpSetPixel (x, y) value = return () -- TODO: Implement SetPixel support
    mpDebug log = do
        liftIO $ putStrLn log


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
            (cpu', _) <- CPU.mDispatcher CPU.tick mos6502
            put nes{cpu = cpu'}
        else put nes

tickPPU :: StateT NES IO ()
tickPPU = do
    nes <- get
    let r2c02 = ppu nes
    (ppu', _) <- PPU.mDispatcher PPU.tick r2c02
    put nes{ppu = ppu'}

tick :: StateT NES IO ()
tick = do
    nes <- get
    liftIO . putStrLn $ "Debug: " ++ (PPU.cdebug . PPU.context . ppu $ nes)
    nes <- get
    tickCPU
    --tickPPU
    updateClock 1

reset :: StateT NES IO ()
reset = do
    nes <- get
    (cpu', _) <- CPU.mDispatcher CPU.reset (cpu nes)
    (ppu', _) <- PPU.mDispatcher PPU.reset (ppu nes)
    --cart' <- liftIO $ Cart.reset $ cartridge nes
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


