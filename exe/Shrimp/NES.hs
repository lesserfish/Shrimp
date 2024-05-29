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
import qualified Shrimp.Display as Display

data NESContext = NESContext
    { nCPUComplete :: Bool
    , nPPUComplete :: Bool
    }

data NES = NES
    { cpu :: !CPU.MOS6502
    , ppu :: !PPU.R2C02
    , cartridge :: !Cart.Cartridge
    , cpuRAM :: !Memory.RAM
    , nametableRAM :: !Memory.RAM
    , paletteRAM :: !Memory.RAM
    , context :: !NESContext
    , nClock :: !Int
    , nDisplay :: !Display.Display
    }

instance Show NES where
    show nes = "Clock: " ++ show (nClock nes)
            ++ "\nCPU: " ++ show (cpu nes)

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
    liftIO $ Memory.readByte (cpuRAM nes) real_addr

cpuReadPPU :: Word16 -> StateT NES IO Word8
cpuReadPPU addr = do
    nes <- get
    let real_addr = addr .&. 0x0007 -- Addresses 0x2000 to 0x2007 is mirrored through $3FFF
    (liftIO $ runStateT (PPU.cpuRead real_addr) (ppu nes, nes)) >>= flattenPPU'

cpuReadAPU :: Word16 -> StateT NES IO Word8
cpuReadAPU addr = return 0 -- TODO: Implement APU Support

cpuReadControl :: Word16 -> StateT NES IO Word8
cpuReadControl addr = return 0 -- TODO: Implement Control Support

cpuReadCart :: Word16 -> StateT NES IO Word8
cpuReadCart addr = do
    nes <- get
    (liftIO $ Cart.cpuRead (cartridge nes) addr) >>= updateCart' -- The mapper fixes the address by itself

cpuWriteRAM :: Word16 -> Word8 -> StateT NES IO ()
cpuWriteRAM addr byte = do
    nes <- get
    let real_addr = addr .&. 0x07FF -- Addresses 0x000 to 0x07FF is mirrored through $1FFFF
    liftIO $ Memory.writeByte (cpuRAM nes) real_addr byte

cpuWritePPU :: Word16 -> Word8 -> StateT NES IO ()
cpuWritePPU addr byte = do
    let real_addr = addr .&. 0x0007 -- Addresses 0x2000 to 0x2007 is mirrored through $3FFF
    getPPU' >>= (liftIO . execStateT (PPU.cpuWrite real_addr byte)) >>= flattenPPU

cpuWriteAPU :: Word16 -> Word8 -> StateT NES IO ()
cpuWriteAPU addr byte = return () -- TODO: Implement APU Support

cpuWriteControl :: Word16 -> Word8 -> StateT NES IO ()
cpuWriteControl addr byte = return () -- TODO: Implement Control Support

cpuWriteCart :: Word16 -> Word8 -> StateT NES IO ()
cpuWriteCart addr byte = do
    nes <- get
    (liftIO $ Cart.cpuWrite (cartridge nes) addr byte) >>= updateCart -- The mapper fixes the address by itself

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

mirrorNametable :: Cart.Mirroring -> Word16 -> Word16
mirrorNametable Cart.Horizontal addr
    | (addr >= 0x2000 && addr <= 0x23FF) = 0x000
    | (addr >= 0x2400 && addr <= 0x27FF) = 0x400
    | (addr >= 0x2800 && addr <= 0x2BFF) = 0x000
    | (addr >= 0x2C00 && addr <= 0x2FFF) = 0x400
    | otherwise = error "Address out of range"
mirrorNametable Cart.Vertical addr
    | (addr >= 0x2000 && addr <= 0x23FF) = 0x000
    | (addr >= 0x2400 && addr <= 0x27FF) = 0x000
    | (addr >= 0x2800 && addr <= 0x2BFF) = 0x400
    | (addr >= 0x2C00 && addr <= 0x2FFF) = 0x400
    | otherwise = error "Address out of range"

ppuReadPT :: Word16 -> StateT NES IO Word8
ppuReadPT addr = do
    nes <- get
    let cart = cartridge nes
    (liftIO $ Cart.ppuRead cart addr) >>= updateCart'

ppuReadNT :: Word16 -> StateT NES IO Word8
ppuReadNT addr = do
    nes <- get
    let mirroring = Cart.hMirroring . Cart.cHeader . Cart.cartData . cartridge $ nes
    let baseaddr = mirrorNametable mirroring addr
    let addr' = baseaddr + (addr .&. 0x03FF)
    liftIO $ Memory.readByte (nametableRAM nes) addr'

ppuReadPL' :: Word16 -> StateT NES IO Word8
ppuReadPL' 0x04 = ppuReadPL' 0
ppuReadPL' 0x08 = ppuReadPL' 0
ppuReadPL' 0x0C = ppuReadPL' 0
ppuReadPL' 0x10 = ppuReadPL' 0
ppuReadPL' 0x14 = ppuReadPL' 0
ppuReadPL' 0x18 = ppuReadPL' 0
ppuReadPL' 0x1C = ppuReadPL' 0
ppuReadPL' addr = do
    nes <- get
    liftIO $ Memory.readByte (paletteRAM nes) addr

ppuReadPL :: Word16 -> StateT NES IO Word8
ppuReadPL addr = ppuReadPL' (addr .&. 0x1F)

ppuWritePT :: Word16 -> Word8 -> StateT NES IO ()
ppuWritePT addr byte = do
    nes <- get
    let cart = cartridge nes
    (liftIO $ Cart.ppuWrite cart addr byte) >>= updateCart

ppuWriteNT :: Word16 -> Word8 -> StateT NES IO ()
ppuWriteNT addr byte = do
    nes <- get
    let mirroring = Cart.hMirroring . Cart.cHeader . Cart.cartData . cartridge $ nes
    let baseaddr = mirrorNametable mirroring addr
    let addr' = baseaddr + (addr .&. 0x03FF)
    liftIO $ Memory.writeByte (nametableRAM nes) addr' byte

ppuWritePL' :: Word16 -> Word8 -> StateT NES IO ()
ppuWritePL' 0x04 byte = ppuWritePL' 0 byte
ppuWritePL' 0x08 byte = ppuWritePL' 0 byte
ppuWritePL' 0x0C byte = ppuWritePL' 0 byte
ppuWritePL' 0x10 byte = ppuWritePL' 0 byte
ppuWritePL' 0x14 byte = ppuWritePL' 0 byte
ppuWritePL' 0x18 byte = ppuWritePL' 0 byte
ppuWritePL' 0x1C byte = ppuWritePL' 0 byte
ppuWritePL' addr byte = do
    nes <- get
    liftIO $ Memory.writeByte (paletteRAM nes) addr byte

ppuWritePL :: Word16 -> Word8 -> StateT NES IO ()
ppuWritePL addr byte = ppuWritePL' (addr .&. 0x1F) byte

ppuSetPixel :: (Word16, Word16) -> Word8 -> StateT NES IO ()
ppuSetPixel addr col = do
    display <- nDisplay <$> get
    liftIO $ Display.setPixel display addr col

ppuTriggerNMI :: StateT NES IO ()
ppuTriggerNMI = (getCPU' >>= (liftIO . execStateT CPU.iNMI) >>= flattenCPU)
    
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
    pSetPixel addr col nes = exec (ppuSetPixel addr col) nes
    pTriggerNMI nes = exec ppuTriggerNMI nes
    pDebug log nes = do
        liftIO $ putStrLn log
        return nes


flattenCPU :: (CPU.MOS6502, NES) -> StateT NES IO ()
flattenCPU (mos, nes) = put nes{cpu = mos}

flattenCPU' :: (a, (CPU.MOS6502, NES)) -> StateT NES IO a
flattenCPU' (smth, (mos, nes)) = put nes{cpu = mos} >> return smth

flattenPPU :: (PPU.R2C02, NES) -> StateT NES IO ()
flattenPPU (r2c02, nes) = put nes{ppu = r2c02}

flattenPPU' :: (a, (PPU.R2C02, NES)) -> StateT NES IO a
flattenPPU' (smth, (r2c02, nes)) = put nes{ppu = r2c02} >> return smth

updateCart :: Cart.Cartridge -> StateT NES IO ()
updateCart c = modify(\nes -> nes{cartridge = c})

updateCart' :: (Cart.Cartridge, a) -> StateT NES IO a
updateCart' (c, smth) = modify(\nes -> nes{cartridge = c}) >> return smth


updateClock :: Int -> StateT NES IO ()
updateClock offset = modify(\nes -> nes{nClock = (nClock nes) + offset})

tickCPU :: StateT NES IO ()
tickCPU = getCPU' >>= (liftIO . (execStateT CPU.tick)) >>= flattenCPU

tickPPU :: StateT NES IO ()
tickPPU = getPPU' >>= (liftIO . (execStateT PPU.tick)) >>= flattenPPU

tick :: StateT NES IO ()
tick = do
    tickCPU
    tickPPU
    tickPPU
    tickPPU
    updateClock 3

getCPU :: StateT NES IO CPU.MOS6502
getCPU = cpu <$> get

getCPU' :: StateT NES IO (CPU.MOS6502, NES)
getCPU' = (\nes -> (cpu nes, nes)) <$> get

getPPU :: StateT NES IO PPU.R2C02
getPPU = ppu <$> get

getPPU' :: StateT NES IO (PPU.R2C02, NES)
getPPU' = (\nes -> (ppu nes, nes)) <$> get

setCPUComplete :: Bool -> StateT NES IO ()
setCPUComplete b = modify (\nes -> nes{context = (context nes){nCPUComplete = b}})

fetchCPUComplete :: StateT NES IO Bool
fetchCPUComplete = do
    getCPU' >>= (liftIO . runStateT CPU.fetchComplete) >>= flattenCPU'

fetchPPUComplete :: StateT NES IO Bool
fetchPPUComplete = do
    getPPU' >>= (liftIO . runStateT PPU.fetchComplete) >>= flattenPPU'

setPPUComplete :: Bool -> StateT NES IO ()
setPPUComplete b = modify (\nes -> nes{context = (context nes){nPPUComplete = b}})

reset :: StateT NES IO ()
reset = do
    nes <- get
    cpu' <- fst <$> (getCPU' >>= (liftIO . execStateT CPU.reset))
    ppu' <- fst <$> (getPPU' >>= (liftIO . execStateT PPU.reset))
    --cart' <- liftIO $ Cart.reset $ cartridge nes -- TODO: Implement cartridge resetting. Probably just run a fromCartDataIO.
    liftIO $ Memory.reset (cpuRAM nes)
    liftIO $ Memory.reset (nametableRAM nes)
    liftIO $ Memory.reset (paletteRAM nes)
    liftIO $ Display.reset (nDisplay nes)
    let nes' =
            nes
                { cpu = cpu'
                , ppu = ppu'
                --, cartridge = cart'
                , context = NESContext False False
                , nClock = 0
                } 
    put nes'

empty :: IO NES
empty = do
    nocart <- Cart.emptyCartridge
    cpuram <- Memory.new 0x800 0
    nmtbram <- Memory.new 0x800 0
    pltram <- Memory.new 0x20 0
    display <- Display.newDisplay
    return $ NES
        { cpu = CPU.mos6502
        , ppu = PPU.r2c02
        , cartridge = nocart
        , cpuRAM = cpuram
        , nametableRAM = nmtbram
        , paletteRAM = pltram
        , context = NESContext False False
        , nClock = 0
        , nDisplay = display
        }

loadNES :: FilePath -> IO NES
loadNES fp = do
    cart <- Cart.loadCartridge fp
    emptynes <- empty
    let nes' = emptynes{cartridge = cart}
    nes <- execStateT reset nes'
    return nes

