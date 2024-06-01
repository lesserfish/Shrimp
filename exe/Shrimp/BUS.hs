{-# LANGUAGE MultiParamTypeClasses #-}
module Shrimp.BUS (
    BUS(..),
    tick,
    fullTick,
    fullFrame,
    load,
    reset,
    ppuPeek,
    cpuPeek
) where

import Control.Monad (when)
import Control.Monad.State
import Data.Word
import Data.Bits
import qualified Shrimp.Cartridge as Cartridge
import qualified Shrimp.MOS6502 as MOS6502
import qualified Shrimp.Memory as Memory
import qualified Shrimp.R2C02 as R2C02
import qualified Shrimp.Display as Display
import Data.IORef


data BUS = BUS
    { bCPU :: !(IORef MOS6502.MOS6502)
    , bPPU :: !(IORef R2C02.R2C02)
    , bCart :: !Cartridge.Cartridge
    , bRAM :: !Memory.RAM
    , bNTRAM :: !Memory.RAM
    , bPLRAM :: !Memory.RAM
    , bDisplay :: !Display.Display
    }


-- CPU


-- READ
cpuReadRAM :: Memory.RAM -> Word16 -> IO Word8
cpuReadRAM ram addr = do
    let real_addr = addr .&. 0x07FF -- Addresses 0x000 to 0x07FF is mirrored through $1FFFF
    Memory.readByte ram real_addr


cpuReadPPU :: (IORef R2C02.R2C02) -> Word16 -> IO Word8
cpuReadPPU ppuref addr = do
    ppu <- readIORef ppuref
    let real_addr = addr .&. 0x0007 -- Addresses 0x2000 to 0x2007 is mirrored through $3FFF
    (byte, ppu') <- runStateT (R2C02.cpuRead real_addr) ppu
    writeIORef ppuref ppu'
    return byte


cpuReadAPU :: Word16 -> IO Word8
cpuReadAPU addr = return 0 -- TODO: Implement APU Support


cpuReadControl :: Word16 -> IO Word8
cpuReadControl addr = return 0 -- TODO: Implement Control Support


cpuReadCart :: Cartridge.Cartridge -> Word16 -> IO Word8
cpuReadCart cart addr = Cartridge.cpuRead cart addr



-- PEEK

cpuPeekRAM :: Memory.RAM -> Word16 -> IO Word8
cpuPeekRAM = cpuReadRAM


cpuPeekPPU :: (IORef R2C02.R2C02) -> Word16 -> IO Word8
cpuPeekPPU ppuref addr = do
    ppu <- readIORef ppuref
    let real_addr = addr .&. 0x0007 -- Addresses 0x2000 to 0x2007 is mirrored through $3FFF
    byte <- R2C02.cpuPeek real_addr ppu
    return byte


cpuPeekAPU :: Word16 -> IO Word8
cpuPeekAPU addr = return 0 -- TODO: Implement APU Support


cpuPeekControl :: Word16 -> IO Word8
cpuPeekControl addr = return 0 -- TODO: Implement Control Support


cpuPeekCart :: Cartridge.Cartridge -> Word16 -> IO Word8
cpuPeekCart cart addr = Cartridge.cpuPeek cart addr



-- WRITE

cpuWriteRAM :: Memory.RAM -> Word16 -> Word8 -> IO ()
cpuWriteRAM ram addr byte = do
    let real_addr = addr .&. 0x07FF -- Addresses 0x000 to 0x07FF is mirrored through $1FFFF
    Memory.writeByte ram real_addr byte


cpuWritePPU :: (IORef R2C02.R2C02) -> Word16 -> Word8 -> IO ()
cpuWritePPU ppuref addr byte = do
    ppu <- readIORef ppuref
    let real_addr = addr .&. 0x0007 -- Addresses 0x2000 to 0x2007 is mirrored through $3FFF
    ppu' <- execStateT (R2C02.cpuWrite real_addr byte) ppu
    writeIORef ppuref ppu'


cpuWriteAPU :: Word16 -> Word8 -> IO ()
cpuWriteAPU addr byte = return () -- TODO: Implement APU Support


cpuWriteControl :: Word16 -> Word8 -> IO ()
cpuWriteControl addr byte = return () -- TODO: Implement Control Support


cpuWriteCart :: Cartridge.Cartridge -> Word16 -> Word8 -> IO ()
cpuWriteCart cart addr byte = Cartridge.cpuWrite cart addr byte



-- CPU INTERFACE

cpuInterface :: (IORef R2C02.R2C02) -> Memory.RAM -> Cartridge.Cartridge -> MOS6502.Interface
cpuInterface ppuref ram cart = MOS6502.Interface cReadByte cWriteByte cPeekByte where
    cReadByte addr
        | (addr >= 0x0000 && addr <= 0x1FFF) = cpuReadRAM ram addr
        | (addr >= 0x2000 && addr <= 0x3FFF) = cpuReadPPU ppuref addr
        | (addr >= 0x4000 && addr <= 0x4015) = cpuReadAPU addr
        | (addr >= 0x4016 && addr <= 0x4017) = cpuReadControl addr
        | (addr >= 0x4020 && addr <= 0xFFFF) = cpuReadCart cart addr
        | otherwise = return 0 -- TODO: Log error ?
    cWriteByte addr byte
        | (addr >= 0x0000 && addr <= 0x1FFF) = cpuWriteRAM ram addr byte
        | (addr >= 0x2000 && addr <= 0x3FFF) = cpuWritePPU ppuref addr byte
        | (addr >= 0x4000 && addr <= 0x4015) = cpuWriteAPU addr byte
        | (addr >= 0x4016 && addr <= 0x4017) = cpuWriteControl addr byte
        | (addr >= 0x4020 && addr <= 0xFFFF) = cpuWriteCart cart addr byte
        | otherwise = return () -- TODO: Log error ?
    cPeekByte addr
        | (addr >= 0x0000 && addr <= 0x1FFF) = cpuPeekRAM ram addr
        | (addr >= 0x2000 && addr <= 0x3FFF) = cpuPeekPPU ppuref addr
        | (addr >= 0x4000 && addr <= 0x4015) = cpuPeekAPU addr
        | (addr >= 0x4016 && addr <= 0x4017) = cpuPeekControl addr
        | (addr >= 0x4020 && addr <= 0xFFFF) = cpuPeekCart cart addr
        | otherwise = return 0 -- TODO: Log error ?





-- PPU

mirrorNametable :: Cartridge.Mirroring -> Word16 -> Word16
mirrorNametable Cartridge.Horizontal addr
    | (addr >= 0x2000 && addr <= 0x23FF) = 0x000
    | (addr >= 0x2400 && addr <= 0x27FF) = 0x400
    | (addr >= 0x2800 && addr <= 0x2BFF) = 0x000
    | (addr >= 0x2C00 && addr <= 0x2FFF) = 0x400
    | otherwise = error "Address out of range"
mirrorNametable Cartridge.Vertical addr
    | (addr >= 0x2000 && addr <= 0x23FF) = 0x000
    | (addr >= 0x2400 && addr <= 0x27FF) = 0x000
    | (addr >= 0x2800 && addr <= 0x2BFF) = 0x400
    | (addr >= 0x2C00 && addr <= 0x2FFF) = 0x400
    | otherwise = error "Address out of range"



-- READ


ppuReadPT :: Cartridge.Cartridge -> Word16 -> IO Word8
ppuReadPT cart addr = Cartridge.ppuRead cart addr


ppuReadNT :: Cartridge.Cartridge -> Memory.RAM -> Word16 -> IO Word8
ppuReadNT cart ntram addr = do
    let mirroring = Cartridge.hMirroring . Cartridge.cHeader . Cartridge.cartData $ cart
    let baseaddr = mirrorNametable mirroring addr
    let addr' = baseaddr + (addr .&. 0x03FF)
    Memory.readByte ntram addr'


ppuReadPL' :: Memory.RAM -> Word16 -> IO Word8
--ppuReadPL' plram 0x04 = ppuReadPL' plram 0x0
--ppuReadPL' plram 0x08 = ppuReadPL' plram 0x0
--ppuReadPL' plram 0x0C = ppuReadPL' plram 0x0


ppuReadPL' plram 0x10 = ppuReadPL' plram 0x0
ppuReadPL' plram 0x14 = ppuReadPL' plram 0x4
ppuReadPL' plram 0x18 = ppuReadPL' plram 0x8
ppuReadPL' plram 0x1C = ppuReadPL' plram 0xC
ppuReadPL' plram addr = Memory.readByte plram addr


ppuReadPL :: Memory.RAM -> Word16 -> IO Word8
ppuReadPL plram addr = ppuReadPL' plram (addr .&. 0x1F)



-- PEEK

ppuPeekPT :: Cartridge.Cartridge -> Word16 -> IO Word8
ppuPeekPT cart addr = Cartridge.ppuPeek cart addr


ppuPeekNT :: Cartridge.Cartridge -> Memory.RAM -> Word16 -> IO Word8
ppuPeekNT = ppuReadNT


ppuPeekPL :: Memory.RAM -> Word16 -> IO Word8
ppuPeekPL = ppuReadPL



-- WRITE


ppuWritePT :: Cartridge.Cartridge -> Word16 -> Word8 -> IO ()
ppuWritePT cart addr byte = Cartridge.ppuWrite cart addr byte


ppuWriteNT :: Cartridge.Cartridge -> Memory.RAM -> Word16 -> Word8 -> IO ()
ppuWriteNT cart ntram addr byte = do
    let mirroring = Cartridge.hMirroring . Cartridge.cHeader . Cartridge.cartData $ cart
    let baseaddr = mirrorNametable mirroring addr
    let addr' = baseaddr + (addr .&. 0x03FF)
    Memory.writeByte ntram addr' byte


ppuWritePL' :: Memory.RAM -> Word16 -> Word8 ->IO ()
--ppuWritePL' plram 0x04 byte = ppuWritePL' plram 0x00 byte
--ppuWritePL' plram 0x08 byte = ppuWritePL' plram 0x00 byte
--ppuWritePL' plram 0x0C byte = ppuWritePL' plram 0x00 byte

ppuWritePL' plram 0x10 byte = ppuWritePL' plram 0x00 byte
ppuWritePL' plram 0x14 byte = ppuWritePL' plram 0x04 byte
ppuWritePL' plram 0x18 byte = ppuWritePL' plram 0x08 byte
ppuWritePL' plram 0x1C byte = ppuWritePL' plram 0x0C byte
ppuWritePL' plram addr byte = Memory.writeByte plram addr byte

ppuWritePL :: Memory.RAM -> Word16 -> Word8 -> IO ()
ppuWritePL plram addr byte = ppuWritePL' plram (addr .&. 0x1F) byte



-- AUXILIARY


ppuSetPixel :: Display.Display -> (Word16, Word16) -> Word8 -> IO ()
ppuSetPixel display addr col = Display.setPixel display addr col

ppuTriggerNMI :: (IORef MOS6502.MOS6502) -> IO ()
ppuTriggerNMI cpuref = do
    cpu <- readIORef cpuref
    cpu' <- execStateT MOS6502.iNMI cpu
    writeIORef cpuref cpu'
    


-- PPU INTERFACE

ppuInterface :: (IORef MOS6502.MOS6502) -> Cartridge.Cartridge -> Display.Display -> Memory.RAM -> Memory.RAM -> R2C02.Interface
ppuInterface cpuref cart display plram ntram = R2C02.Interface pReadByte pWriteByte pSetPixel pTriggerNMI pPeekByte where
    pReadByte addr
        | (addr >= 0x0000 && addr <= 0x1FFF) = ppuReadPT cart addr
        | (addr >= 0x2000 && addr <= 0x3EFF) = ppuReadNT cart ntram addr
        | (addr >= 0x3F00 && addr <= 0x3FFF) = ppuReadPL plram addr
        | otherwise = return 0 -- TODO: Log error
    pWriteByte addr byte
        | (addr >= 0x0000 && addr <= 0x1FFF) = ppuWritePT cart addr byte
        | (addr >= 0x2000 && addr <= 0x3EFF) = ppuWriteNT cart ntram addr byte
        | (addr >= 0x3F00 && addr <= 0x3FFF) = ppuWritePL plram addr byte
        | otherwise = return ()
    pPeekByte addr
        | (addr >= 0x0000 && addr <= 0x1FFF) = ppuPeekPT cart addr
        | (addr >= 0x2000 && addr <= 0x3EFF) = ppuPeekNT cart ntram addr
        | (addr >= 0x3F00 && addr <= 0x3FFF) = ppuPeekPL plram addr
        | otherwise = return 0
    pSetPixel addr col = ppuSetPixel display addr col
    pTriggerNMI = ppuTriggerNMI cpuref



-- BUS Interface

tickCPU :: BUS -> IO ()
tickCPU bus = do
    let cpuref = bCPU bus
    cpu <- readIORef cpuref
    cpu' <- execStateT MOS6502.tick cpu
    writeIORef cpuref cpu'

tickCPU' :: BUS -> IO Bool
tickCPU' bus = do
    let cpuref = bCPU bus
    cpu <- readIORef cpuref
    (done, cpu') <- runStateT MOS6502.tick' cpu
    writeIORef cpuref cpu'
    return $ done


tickPPU :: BUS -> IO ()
tickPPU bus = do
    let ppuref = bPPU bus
    ppu <- readIORef ppuref
    ppu' <- execStateT R2C02.tick ppu
    writeIORef ppuref ppu'

tickPPU' :: BUS -> IO ()
tickPPU' bus = do
    let ppuref = bPPU bus
    ppu <- readIORef ppuref
    (done, ppu') <- runStateT R2C02.tick ppu
    writeIORef ppuref ppu'
    return done


tickNPPU :: Int -> BUS -> IO ()
tickNPPU n bus = do
    let ppuref = bPPU bus
    ppu <- readIORef ppuref
    let action = mapM_(\_ -> R2C02.tick) [1..n]
    ppu' <- execStateT action ppu
    writeIORef ppuref ppu'

tickNPPU' :: Int -> BUS -> IO Bool
tickNPPU' n bus = do
    let ppuref = bPPU bus
    ppu <- readIORef ppuref
    let action = (mapM_(\_ -> R2C02.tick) [1..(n-1)]) >> R2C02.tick'
    (done, ppu') <- runStateT action ppu
    writeIORef ppuref ppu'
    return done


tick :: BUS -> IO ()
tick bus = do
    tickNPPU 3 bus
    tickCPU bus

fullTick :: BUS -> IO ()
fullTick bus = do
    tickPPU bus
    tickPPU bus
    tickPPU bus
    done <- tickCPU' bus
    if done then return () else fullTick bus
   
fullFrame :: BUS -> IO ()
fullFrame bus = do
    done <- tickNPPU' 3 bus
    tickCPU bus
    if done then return () else fullFrame bus

load :: FilePath -> IO BUS
load fp = do
    cart <- Cartridge.loadCartridge fp
    ram <- Memory.new 0x800 0
    ntram <- Memory.new 0x800 0
    plram <- Memory.new 0x20 0
    display <- Display.newDisplay
    cpuref <- newIORef undefined :: IO (IORef MOS6502.MOS6502)
    ppuref <- newIORef undefined :: IO (IORef R2C02.R2C02)

    let ppuinterface = ppuInterface cpuref cart display plram ntram
    let cpuinterface = cpuInterface ppuref ram cart

    let cpu = MOS6502.new cpuinterface
    let ppu = R2C02.new ppuinterface

    writeIORef cpuref cpu
    writeIORef ppuref ppu


    let bus = BUS { bCPU = cpuref
                  , bPPU = ppuref
                  , bCart = cart
                  , bRAM = ram
                  , bNTRAM = ntram
                  , bPLRAM = plram
                  , bDisplay = display
                  }
    reset bus
    return bus


reset :: BUS -> IO ()
reset bus = do
    Memory.reset (bRAM bus)
    Memory.reset (bNTRAM bus)
    Memory.reset (bPLRAM bus)
    Display.reset (bDisplay bus)
    Cartridge.reset (bCart bus) -- TODO
    let cpuref = bCPU bus
    let ppuref = bPPU bus
    cpu <- readIORef cpuref
    ppu <- readIORef ppuref
    cpu' <- execStateT MOS6502.reset cpu
    ppu' <- execStateT R2C02.reset ppu
    writeIORef cpuref cpu'
    writeIORef ppuref ppu'


ppuPeek :: BUS -> Word16 -> IO Word8
ppuPeek bus addr = do
   let ppuref = bPPU bus 
   ppu <- readIORef ppuref
   let peeker = R2C02.iPeekByte . R2C02.interface $ ppu
   peeker addr


cpuPeek :: BUS -> Word16 -> IO Word8
cpuPeek bus addr = do
   let cpuref = bCPU bus 
   cpu <- readIORef cpuref
   let peeker = MOS6502.iPeekByte . MOS6502.interface $ cpu
   peeker addr
