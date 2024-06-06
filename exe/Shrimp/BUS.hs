{-# LANGUAGE MultiParamTypeClasses #-}
module Shrimp.BUS (
    BUS(..),
    tick,
    fullTick,
    fullFrame,
    load,
    reset,
    setControllerA,
    setControllerB,
    ppuPeek,
    cpuPeek
) where

import Shrimp.Utils
import Control.Monad (when)
import Control.Monad.State
import Data.Word
import Data.Bits
import qualified Shrimp.Cartridge as Cartridge
import qualified Shrimp.MOS6502 as MOS6502
import qualified Shrimp.Memory as Memory
import qualified Shrimp.R2C02 as R2C02
import qualified Shrimp.Display as Display
import qualified Shrimp.Controller as Controller
import Data.IORef
import SDL (R2)


data Context = Context
    { dmaPage :: Word8
    , dmaByte :: Word8
    , dmaCycle :: Int
    , dmaHold :: Bool
    , bClock :: Int
    }

data BUS = BUS
    { bCPU :: !(MOS6502.MOS6502 BUS)
    , bPPU :: !R2C02.R2C02
    , bContext :: !Context
    , bCart :: !Cartridge.Cartridge
    , bRAM :: !Memory.RAM
    , bNTRAM :: !Memory.RAM
    , bPLRAM :: !Memory.RAM
    , bDisplay :: !Display.Display
    , bControllerA :: !Controller.Controller
    , bControllerB :: !Controller.Controller
    }

data Helper = Helper
    { helperCart :: !Cartridge.Cartridge
    , helperRAM :: !Memory.RAM
    , helperNTRAM :: !Memory.RAM
    , helperPLRAM :: !Memory.RAM
    , helperDisplay :: !Display.Display
    , helperControllerA :: !Controller.Controller
    , helperControllerB :: !Controller.Controller
    }


-- Setters / Getters

getDMAPage :: StateT BUS IO Word8
getDMAPage = dmaPage . bContext <$> get

getDMAByte :: StateT BUS IO Word8
getDMAByte = dmaByte . bContext <$> get

getDMACycle :: StateT BUS IO Int
getDMACycle = dmaCycle . bContext <$> get

getDMAHold :: StateT BUS IO Bool
getDMAHold = dmaHold . bContext <$> get

getClock :: StateT BUS IO Int
getClock = bClock . bContext <$> get

setDMAPage :: Word8 -> StateT BUS IO ()
setDMAPage v = modify (\bus -> bus{bContext = (bContext bus){dmaPage = v}})

setDMAByte :: Word8 -> StateT BUS IO ()
setDMAByte v = modify (\bus -> bus{bContext = (bContext bus){dmaByte = v}})

setDMACycle :: Int -> StateT BUS IO ()
setDMACycle v = modify (\bus -> bus{bContext = (bContext bus){dmaCycle = v}})

setDMAHold :: Bool -> StateT BUS IO ()
setDMAHold v = modify (\bus -> bus{bContext = (bContext bus){dmaHold = v}})

setClock :: Int -> StateT BUS IO ()
setClock v = modify (\bus -> bus{bContext = (bContext bus){bClock = v}})

incDMACycle :: StateT BUS IO ()
incDMACycle = (( + 1) <$> getDMACycle) >>= setDMACycle

getPPU :: StateT BUS IO R2C02.R2C02
getPPU = bPPU <$> get

getCPU :: StateT BUS IO (MOS6502.MOS6502 BUS)
getCPU = bCPU <$> get

setPPU :: R2C02.R2C02 -> StateT BUS IO ()
setPPU ppu = modify(\bus -> bus{bPPU = ppu})

setCPU :: (MOS6502.MOS6502 BUS) -> StateT BUS IO ()
setCPU cpu = modify(\bus -> bus{bCPU = cpu})


-- CPU


-- READ
cpuReadRAM :: BUS -> Word16 -> IO (BUS, Word8)
cpuReadRAM bus addr = do
    let ram = bRAM bus
    let real_addr = addr .&. 0x07FF -- Addresses 0x000 to 0x07FF is mirrored through $1FFFF
    byte <- Memory.readByte ram real_addr
    return $ (bus, byte)


cpuReadPPU :: BUS -> Word16 -> IO (BUS, Word8)
cpuReadPPU bus addr = do
    let ppu = bPPU bus 
    let real_addr = addr .&. 0x0007 -- Addresses 0x2000 to 0x2007 is mirrored through $3FFF
    (byte, ppu') <- runStateT (R2C02.cpuRead real_addr) ppu
    let bus' = bus{bPPU = ppu'}
    return (bus', byte)


cpuReadAPU :: BUS -> Word16 -> IO (BUS, Word8)
cpuReadAPU bus addr = return $ (bus, 0) -- TODO: Implement APU Support


cpuReadControl :: BUS -> Word16 -> IO (BUS, Word8)
cpuReadControl bus addr = do
    let addr' = addr .&. 0x01
    let controller = if addr' == 0 then (bControllerA bus) else (bControllerB bus)
    byte <- Controller.readController controller
    return (bus, byte)
    

cpuReadCart :: BUS -> Word16 -> IO (BUS, Word8)
cpuReadCart bus addr = do
    let cart = bCart bus
    byte <- Cartridge.cpuRead cart addr
    return $ (bus, byte)



-- PEEK

cpuPeekRAM :: BUS -> Word16 -> IO Word8
cpuPeekRAM bus addr = snd <$> cpuReadRAM bus addr


cpuPeekPPU :: BUS -> Word16 -> IO Word8
cpuPeekPPU bus addr = do
    let ppu = bPPU bus
    let real_addr = addr .&. 0x0007 -- Addresses 0x2000 to 0x2007 is mirrored through $3FFF
    byte <- R2C02.cpuPeek real_addr ppu
    return byte


cpuPeekAPU :: BUS -> Word16 -> IO Word8
cpuPeekAPU bus addr = return 0 -- TODO: Implement APU Support


cpuPeekControl :: BUS -> Word16 -> IO Word8
cpuPeekControl bus addr = return 0 -- TODO: Implement Control Support


cpuPeekCart :: BUS -> Word16 -> IO Word8
cpuPeekCart bus addr = do
    let cart = bCart bus
    Cartridge.cpuPeek cart addr



-- WRITE

cpuWriteRAM :: BUS -> Word16 -> Word8 -> IO BUS
cpuWriteRAM bus addr byte = do
    let ram = bRAM bus
    let real_addr = addr .&. 0x07FF -- Addresses 0x000 to 0x07FF is mirrored through $1FFFF
    Memory.writeByte ram real_addr byte
    return bus


cpuWritePPU :: BUS -> Word16 -> Word8 -> IO BUS
cpuWritePPU bus addr byte = do
    let ppu = bPPU bus
    let real_addr = addr .&. 0x0007 -- Addresses 0x2000 to 0x2007 is mirrored through $3FFF
    ppu' <- execStateT (R2C02.cpuWrite real_addr byte) ppu
    let bus' = bus{bPPU = ppu'}
    return bus'

-- cpuTriggerDMA: This is not particularly correct.
-- When the CPU triggers a DMA, the bus will wait for 1 or 2 clock cycles before starting the copy,
-- which will take place over a duration of 512 clock ticks.
-- The correct thing would be to set dmaCycle = if mod clock 2 == 0 then (-1) else (-2),
-- and then, in tickDMA, check whether or not dmaCycle is positive.
cpuTriggerDMA :: BUS -> Word8 -> IO BUS
cpuTriggerDMA bus byte = do
    let ctx = (bContext bus){dmaPage = byte, dmaByte = 0, dmaCycle = 0, dmaHold = True} 
    let bus' = bus{bContext = ctx}
    return bus'

cpuWriteAPU :: BUS -> Word16 -> Word8 -> IO BUS
cpuWriteAPU bus addr byte = return bus -- TODO: Implement APU Support


cpuWriteControl :: BUS -> Word16 -> Word8 -> IO BUS
cpuWriteControl bus addr byte = do
    let addr' = addr .&. 0x01
    let controller = if addr' == 0 then (bControllerA bus) else (bControllerB bus)
    Controller.writeController controller
    return bus


cpuWriteCart :: BUS -> Word16 -> Word8 -> IO BUS
cpuWriteCart bus addr byte = do
    let cart = bCart bus
    Cartridge.cpuWrite cart addr byte
    return bus


-- CPU INTERFACE

cReadByte :: BUS -> Word16 -> IO (BUS, Word8)
cReadByte bus addr
    | (addr >= 0x0000 && addr <= 0x1FFF) = cpuReadRAM bus addr
    | (addr >= 0x2000 && addr <= 0x3FFF) = cpuReadPPU bus addr
    | (addr >= 0x4000 && addr <= 0x4015) = cpuReadAPU bus addr
    | (addr >= 0x4016 && addr <= 0x4017) = cpuReadControl bus addr
    | (addr >= 0x4020 && addr <= 0xFFFF) = cpuReadCart bus addr
    | otherwise = return (bus, 0) -- TODO: Log error ?
cWriteByte :: BUS -> Word16 -> Word8 -> IO BUS
cWriteByte bus addr byte
    | (addr >= 0x0000 && addr <= 0x1FFF) = cpuWriteRAM bus addr byte
    | (addr >= 0x2000 && addr <= 0x3FFF) = cpuWritePPU bus addr byte
    | addr == 0x4014                     = cpuTriggerDMA bus byte
    | (addr >= 0x4000 && addr <= 0x4015) = cpuWriteAPU bus addr byte 
    | (addr >= 0x4016 && addr <= 0x4017) = cpuWriteControl bus addr byte
    | (addr >= 0x4020 && addr <= 0xFFFF) = cpuWriteCart bus addr byte
    | otherwise = return bus -- TODO: Log error ?
cPeekByte :: BUS -> Word16 -> IO Word8
cPeekByte bus addr
    | (addr >= 0x0000 && addr <= 0x1FFF) = cpuPeekRAM bus addr
    | (addr >= 0x2000 && addr <= 0x3FFF) = cpuPeekPPU bus addr
    | (addr >= 0x4000 && addr <= 0x4015) = cpuPeekAPU bus addr
    | (addr >= 0x4016 && addr <= 0x4017) = cpuPeekControl bus addr
    | (addr >= 0x4020 && addr <= 0xFFFF) = cpuPeekCart bus addr
    | otherwise = return 0 -- TODO: Log error ?

cpuInterface :: MOS6502.Interface BUS
cpuInterface = MOS6502.Interface cReadByte cWriteByte cPeekByte



-- PPU

mirrorNametable :: Cartridge.Mirroring -> Word16 -> Word16
mirrorNametable Cartridge.Horizontal addr
    | (addr >= 0x2000 && addr <= 0x23FF) = 0x000
    | (addr >= 0x2400 && addr <= 0x27FF) = 0x400
    | (addr >= 0x2800 && addr <= 0x2BFF) = 0x000
    | (addr >= 0x2C00 && addr <= 0x2FFF) = 0x400
    | otherwise = error ("Nametable Address (" ++ toHex2 addr ++ ") out of range")
mirrorNametable Cartridge.Vertical addr
    | (addr >= 0x2000 && addr <= 0x23FF) = 0x000
    | (addr >= 0x2400 && addr <= 0x27FF) = 0x000
    | (addr >= 0x2800 && addr <= 0x2BFF) = 0x400
    | (addr >= 0x2C00 && addr <= 0x2FFF) = 0x400
    | otherwise = error ("Nametable Address (" ++ toHex2 addr ++ ") out of range")



-- READ


ppuReadPT :: Helper -> Word16 -> IO Word8
ppuReadPT bus addr = do
    let cart = helperCart bus
    byte <- Cartridge.ppuRead cart addr
    return byte


ppuReadNT :: Helper -> Word16 -> IO Word8
ppuReadNT bus addr = do
    let cart = helperCart bus
    let ntram = helperNTRAM bus
    let mirroring = Cartridge.hMirroring . Cartridge.cHeader . Cartridge.cartData $ cart
    let baseaddr = mirrorNametable mirroring addr
    let addr' = baseaddr + (addr .&. 0x03FF)
    byte <- Memory.readByte ntram addr'
    return byte


ppuReadPL' :: Memory.RAM -> Word16 -> IO Word8
--ppuReadPL' plram 0x04 = ppuReadPL' plram 0x0
--ppuReadPL' plram 0x08 = ppuReadPL' plram 0x0
--ppuReadPL' plram 0x0C = ppuReadPL' plram 0x0


ppuReadPL' plram 0x10 = ppuReadPL' plram 0x0
ppuReadPL' plram 0x14 = ppuReadPL' plram 0x4
ppuReadPL' plram 0x18 = ppuReadPL' plram 0x8
ppuReadPL' plram 0x1C = ppuReadPL' plram 0xC
ppuReadPL' plram addr = Memory.readByte plram addr


ppuReadPL :: Helper -> Word16 -> IO Word8
ppuReadPL bus addr = do
    let plram = helperPLRAM bus
    byte <- ppuReadPL' plram (addr .&. 0x1F)
    return byte



-- PEEK

ppuPeekPT :: Helper -> Word16 -> IO Word8
ppuPeekPT bus addr = do
    let cart = helperCart bus
    Cartridge.ppuPeek cart addr


ppuPeekNT :: Helper -> Word16 -> IO Word8
ppuPeekNT bus addr = ppuReadNT bus addr


ppuPeekPL :: Helper -> Word16 -> IO Word8
ppuPeekPL bus addr = ppuReadPL bus addr



-- WRITE


ppuWritePT :: Helper -> Word16 -> Word8 -> IO ()
ppuWritePT bus addr byte = do
    let cart = helperCart bus
    Cartridge.ppuWrite cart addr byte


ppuWriteNT :: Helper -> Word16 -> Word8 -> IO ()
ppuWriteNT bus addr byte = do
    let cart = helperCart bus
    let ntram = helperNTRAM bus
    let mirroring = Cartridge.hMirroring . Cartridge.cHeader . Cartridge.cartData $ cart
    let baseaddr = mirrorNametable mirroring addr
    let addr' = baseaddr + (addr .&. 0x03FF)
    Memory.writeByte ntram addr' byte


ppuWritePL' :: Memory.RAM -> Word16 -> Word8 -> IO ()
--ppuWritePL' plram 0x04 byte = ppuWritePL' plram 0x00 byte
--ppuWritePL' plram 0x08 byte = ppuWritePL' plram 0x00 byte
--ppuWritePL' plram 0x0C byte = ppuWritePL' plram 0x00 byte

ppuWritePL' plram 0x10 byte = ppuWritePL' plram 0x00 byte
ppuWritePL' plram 0x14 byte = ppuWritePL' plram 0x04 byte
ppuWritePL' plram 0x18 byte = ppuWritePL' plram 0x08 byte
ppuWritePL' plram 0x1C byte = ppuWritePL' plram 0x0C byte
ppuWritePL' plram addr byte = Memory.writeByte plram addr byte

ppuWritePL :: Helper -> Word16 -> Word8 -> IO ()
ppuWritePL bus addr byte = do
    let plram = helperPLRAM bus
    ppuWritePL' plram (addr .&. 0x1F) byte


ppuSetPixel :: Helper -> (Word16, Word16) -> Word8 -> IO ()
ppuSetPixel bus addr col = do
    let display = helperDisplay bus
    Display.setPixel display addr col

-- PPU INTERFACE

pReadByte :: Helper -> Word16 -> IO Word8
pReadByte bus addr
    | (addr >= 0x0000 && addr <= 0x1FFF) = ppuReadPT bus addr
    | (addr >= 0x2000 && addr <= 0x3EFF) = ppuReadNT bus addr
    | (addr >= 0x3F00 && addr <= 0x3FFF) = ppuReadPL bus addr
    | otherwise = return 0 -- TODO: Log error
pWriteByte :: Helper -> Word16 -> Word8 -> IO ()
pWriteByte bus addr byte
    | (addr >= 0x0000 && addr <= 0x1FFF) = ppuWritePT bus addr byte
    | (addr >= 0x2000 && addr <= 0x3EFF) = ppuWriteNT bus addr byte
    | (addr >= 0x3F00 && addr <= 0x3FFF) = ppuWritePL bus addr byte
    | otherwise = return ()
pPeekByte :: Helper -> Word16 -> IO Word8
pPeekByte bus addr
    | (addr >= 0x0000 && addr <= 0x1FFF) = ppuPeekPT bus addr
    | (addr >= 0x2000 && addr <= 0x3EFF) = ppuPeekNT bus addr
    | (addr >= 0x3F00 && addr <= 0x3FFF) = ppuPeekPL bus addr
    | otherwise = return 0
pSetPixel :: Helper -> (Word16, Word16) -> Word8 -> IO ()
pSetPixel = ppuSetPixel

ppuInterface :: Helper -> R2C02.Interface
ppuInterface bus = R2C02.Interface reader writer painter peeker
    where
        reader = pReadByte bus
        writer = pWriteByte bus
        painter = pSetPixel bus
        peeker = pPeekByte bus



-- BUS Interface

cpuWrite :: Word16 -> Word8 -> StateT BUS IO () 
cpuWrite addr byte = do
    bus <- get
    let cpu = bCPU bus
    let writer = MOS6502.iWriteByte . MOS6502.interface $ cpu
    bus' <- liftIO $ writer bus addr byte
    put bus'


cpuRead :: Word16 -> StateT BUS IO Word8
cpuRead addr = do
    bus <- get
    let cpu = bCPU bus
    let reader = MOS6502.iReadByte . MOS6502.interface $ cpu
    (bus', byte) <- liftIO $ reader bus addr
    put bus'
    return byte

tickDMAR :: Word16 -> StateT BUS IO Bool
tickDMAR offset = do
    page <- fromIntegral <$> getDMAPage
    let addr = 0x100 * page + offset
    byte <- cpuRead addr
    setDMAByte byte
    incDMACycle
    return False


tickDMAW :: Word16 -> StateT BUS IO Bool
tickDMAW offset = do
    byte <- getDMAByte
    ppu <- getPPU
    ppu' <- liftIO $ execStateT (R2C02.dmaPort offset byte) ppu
    setPPU ppu'
    incDMACycle
    let done = offset == 255
    when done (setDMAHold False)
    return $ done

tickDMA :: StateT BUS IO Bool
tickDMA = do
    dmaCycle <- getDMACycle
    let offset = fromIntegral $ dmaCycle `div` 2
    if mod dmaCycle 2 == 0 
       then tickDMAR offset 
       else tickDMAW offset

tickCPU :: StateT BUS IO Bool
tickCPU = do
    bus <- get
    let cpu = bCPU bus
    (done, (cpu', bus')) <- liftIO $ runStateT MOS6502.tick' (cpu, bus)
    put bus'{bCPU = cpu'}
    return done


triggerNMI :: StateT BUS IO ()
triggerNMI = do
    bus <- get
    let cpu = bCPU bus
    (cpu', bus') <- liftIO $ execStateT MOS6502.iNMI (cpu, bus)
    put bus'{bCPU = cpu'}

tick3PPU :: StateT BUS IO Bool
tick3PPU = do
    bus <- get
    let ppu = bPPU bus
    let action = R2C02.tick >> R2C02.tick >> R2C02.tick'
    ((nmi, done), ppu') <- liftIO $ runStateT action ppu
    put bus{bPPU = ppu'}
    when nmi triggerNMI
    return done


chooseTick :: StateT BUS IO Bool
chooseTick = do
    hold <- getDMAHold
    if hold then tickDMA else tickCPU

tick' :: StateT BUS IO (Bool, Bool)
tick' = do
    ppuDone <- tick3PPU
    cpuDone <- chooseTick
    return (ppuDone, cpuDone)


tick :: BUS -> IO ((Bool, Bool), BUS)
tick bus = runStateT tick' bus

fullTick' :: StateT BUS IO Bool
fullTick' = do
    (ppuDone, cpuDone) <- tick'
    if cpuDone then return ppuDone else fullTick'

fullTick :: BUS -> IO (Bool, BUS)
fullTick bus = runStateT fullTick' bus

fullFrame' :: StateT BUS IO ()
fullFrame' = do
    (ppuDone, _) <- tick'
    if ppuDone then return () else fullFrame'

fullFrame :: BUS -> IO BUS
fullFrame bus = execStateT fullFrame' bus


load :: FilePath -> IO BUS
load fp = do
    cart <- Cartridge.loadCartridge fp
    ram <- Memory.new 0x800 0
    ntram <- Memory.new 0x800 0
    plram <- Memory.new 0x20 0
    display <- Display.newDisplay
    controllerA <- Controller.new
    controllerB <- Controller.new

    let helper = Helper { helperCart = cart
                        , helperRAM = ram
                        , helperNTRAM = ntram
                        , helperPLRAM = plram
                        , helperDisplay = display
                        , helperControllerA = controllerA
                        , helperControllerB = controllerB
                        }

    let cpu = MOS6502.new cpuInterface
    ppu <- R2C02.new (ppuInterface helper)

    let ctx = Context 0 0 0 False 0

    let bus = BUS { bCPU = cpu
                  , bPPU = ppu
                  , bContext = ctx
                  , bCart = cart
                  , bRAM = ram
                  , bNTRAM = ntram
                  , bPLRAM = plram
                  , bDisplay = display
                  , bControllerA = controllerA
                  , bControllerB = controllerB
                  }
    bus' <- reset bus
    return bus'

setControllerA :: BUS -> Word8 -> IO()
setControllerA bus controller = Controller.writeLive (bControllerA bus) controller

setControllerB :: BUS -> Word8 -> IO()
setControllerB bus controller = Controller.writeLive (bControllerA bus) controller

resetMemories :: StateT BUS IO ()
resetMemories = do
    bus <- get
    liftIO $ Memory.reset (bRAM bus)
    liftIO $ Memory.reset (bNTRAM bus)
    liftIO $ Memory.reset (bPLRAM bus)
    liftIO $ Display.reset (bDisplay bus)
    liftIO $ Cartridge.reset (bCart bus) -- TODO
    liftIO $ Controller.reset (bControllerA bus) -- TODO
    liftIO $ Controller.reset (bControllerB bus) -- TODO


resetCPU :: StateT BUS IO ()
resetCPU = do
    bus <- get
    let cpu = bCPU bus
    (cpu', bus') <- liftIO $ execStateT MOS6502.reset (cpu, bus)
    put bus'{bCPU = cpu'}

resetPPU :: StateT BUS IO ()
resetPPU = do
    bus <- get
    let ppu = bPPU bus
    ppu' <- liftIO $ execStateT R2C02.reset ppu
    put bus{bPPU = ppu'}

reset' :: StateT BUS IO ()
reset' = do
    resetMemories
    resetCPU
    resetPPU

reset :: BUS -> IO BUS
reset bus = execStateT reset' bus


ppuPeek :: BUS -> Word16 -> IO Word8
ppuPeek bus addr = do
   let ppu = bPPU bus 
   let peeker = R2C02.iPeekByte . R2C02.interface $ ppu
   peeker addr


cpuPeek :: BUS -> Word16 -> IO Word8
cpuPeek bus addr = do
   let cpu = bCPU bus
   let peeker = MOS6502.iPeekByte . MOS6502.interface $ cpu
   peeker bus addr
