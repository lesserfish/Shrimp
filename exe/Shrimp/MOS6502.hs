module Shrimp.MOS6502 (
    Interface (..),
    Registers (..),
    Context (..),
    MOS6502 (..),
    new,
    reset,
    iIRQ,
    iNMI,
    disassemble,
    disassembleL,
    disassembleM,
    disassembleL',
    disassembleM',
    tick,
    tick'
) where

import Shrimp.Utils
import Control.Monad
import Control.Monad.State
import Data.Bits
import Data.Int
import qualified Data.Map as Map
import Data.Word
import Numeric (showHex)
import Text.Printf

data Interface = Interface
    { iReadByte :: Word16 -> IO Word8
    , iWriteByte :: Word16 -> Word8 -> IO ()
    , iPeekByte :: Word16 -> IO Word8
    }

data Registers = Registers
    { pc :: !Word16
    , sp :: !Word8
    , acc :: !Word8
    , idx :: !Word8
    , idy :: !Word8
    , ps :: !Word8
    }

data Context = Context
    { complete :: Bool 
    }
    deriving (Show)

data FLAG
    = CARRY
    | ZERO
    | INTERRUPT_DISABLE
    | DECIMAL_MODE
    | BREAK_CMD
    | OVERFLOW
    | NEGATIVE
    deriving (Show)

data ADDR_MODE
    = IMPLICIT
    | ACCUMULATOR
    | IMMEDIATE
    | ZEROPAGE
    | ZEROPAGE_X
    | ZEROPAGE_Y
    | RELATIVE
    | ABSOLUTE
    | ABSOLUTE_X
    | ABSOLUTE_Y
    | INDIRECT
    | INDIRECT_X
    | INDIRECT_Y
    deriving (Show)

data MOS6502 = MOS6502
    { registers :: !Registers
    , clock :: !Int
    , cycles :: !Int
    , context :: !Context
    , interface :: !Interface
    }


-- Creation


new :: Interface -> MOS6502
new interface = MOS6502 reg 0 0 ctx interface where
    reg = Registers 0 0 0 0 0 0
    ctx = Context False

-- Setters / Getters


mapPC :: (Word16 -> Word16) -> StateT MOS6502 IO ()
mapPC f = modify (\mos -> mos{registers = (registers mos){pc = f . pc . registers $ mos}})


mapSP :: (Word8 -> Word8) -> StateT MOS6502 IO ()
mapSP f = modify (\mos -> mos{registers = (registers mos){sp = f . sp . registers $ mos}})


mapACC :: (Word8 -> Word8) -> StateT MOS6502 IO ()
mapACC f = modify (\mos -> mos{registers = (registers mos){acc = f . acc . registers $ mos}})


mapIDX :: (Word8 -> Word8) -> StateT MOS6502 IO ()
mapIDX f = modify (\mos -> mos{registers = (registers mos){idx = f . idx . registers $ mos}})


mapIDY :: (Word8 -> Word8) -> StateT MOS6502 IO ()
mapIDY f = modify (\mos -> mos{registers = (registers mos){idy = f . idy . registers $ mos}})


mapPS :: (Word8 -> Word8) -> StateT MOS6502 IO ()
mapPS f = modify (\mos -> mos{registers = (registers mos){ps = f . ps . registers $ mos}})



setPC :: Word16 -> StateT MOS6502 IO ()
setPC v = mapPC (\_ -> v)


setSP :: Word8 -> StateT MOS6502 IO ()
setSP v = mapSP (\_ -> v)


setACC :: Word8 -> StateT MOS6502 IO ()
setACC v = mapACC (\_ -> v)


setIDX :: Word8 -> StateT MOS6502 IO ()
setIDX v = mapIDX (\_ -> v)


setIDY :: Word8 -> StateT MOS6502 IO ()
setIDY v = mapIDY (\_ -> v)


setPS :: Word8 -> StateT MOS6502 IO ()
setPS v = mapPS (\_ -> v)



setPCIf :: Bool -> Word16 -> StateT MOS6502 IO ()
setPCIf condition v = if condition then setPC v else return ()


setSPIf :: Bool -> Word8 -> StateT MOS6502 IO ()
setSPIf condition v = if condition then setSP v else return ()


setACCIf :: Bool -> Word8 -> StateT MOS6502 IO ()
setACCIf condition v = if condition then setACC v else return ()


setIDXIf :: Bool -> Word8 -> StateT MOS6502 IO ()
setIDXIf condition v = if condition then setIDX v else return ()


setIDYIf :: Bool -> Word8 -> StateT MOS6502 IO ()
setIDYIf condition v = if condition then setIDY v else return ()


setPSIf :: Bool -> Word8 -> StateT MOS6502 IO ()
setPSIf condition v = if condition then setPS v else return ()



getPC :: StateT MOS6502 IO Word16 
getPC = (pc . registers) <$> get


getSP :: StateT MOS6502 IO Word8 
getSP = (sp . registers) <$> get


getACC :: StateT MOS6502 IO Word8 
getACC = (acc . registers) <$> get


getIDX :: StateT MOS6502 IO Word8 
getIDX = (idx . registers) <$> get


getIDY :: StateT MOS6502 IO Word8 
getIDY = (idy . registers) <$> get


getPS :: StateT MOS6502 IO Word8 
getPS = (ps . registers) <$> get


getFlag :: FLAG -> StateT MOS6502 IO Bool
getFlag CARRY             = b0 <$> getPS
getFlag ZERO              = b1 <$> getPS
getFlag INTERRUPT_DISABLE = b2 <$> getPS
getFlag DECIMAL_MODE      = b3 <$> getPS
getFlag BREAK_CMD         = b4 <$> getPS
getFlag OVERFLOW          = b6 <$> getPS
getFlag NEGATIVE          = b7 <$> getPS


setFlag :: FLAG -> Bool -> StateT MOS6502 IO ()
setFlag CARRY flag             = mapPS (\reg -> if flag then setBit reg 0 else clearBit reg 0)
setFlag ZERO flag              = mapPS (\reg -> if flag then setBit reg 1 else clearBit reg 1)
setFlag INTERRUPT_DISABLE flag = mapPS (\reg -> if flag then setBit reg 2 else clearBit reg 2)
setFlag DECIMAL_MODE flag      = mapPS (\reg -> if flag then setBit reg 3 else clearBit reg 3)
setFlag BREAK_CMD flag         = mapPS (\reg -> if flag then setBit reg 4 else clearBit reg 4)
setFlag OVERFLOW flag          = mapPS (\reg -> if flag then setBit reg 6 else clearBit reg 6)
setFlag NEGATIVE flag          = mapPS (\reg -> if flag then setBit reg 7 else clearBit reg 7)


setFlagIf :: Bool -> FLAG -> Bool -> StateT MOS6502 IO ()
setFlagIf condition flag value = when condition (setFlag flag value)


readByte :: Word16 -> StateT MOS6502 IO Word8
readByte addr = do
    mos6502 <- get
    let read = iReadByte . interface $ mos6502
    byte <- lift $ read addr
    return byte


writeByte :: Word16 -> Word8 -> StateT MOS6502 IO ()
writeByte addr byte = do
    mos6502 <- get
    let write = iWriteByte . interface $ mos6502
    liftIO $ write addr byte



-- Addresing Modes


getAddr :: ADDR_MODE -> StateT MOS6502 IO Word16
getAddr IMPLICIT = return 0 -- Implicit does not require getAddr
getAddr ACCUMULATOR = return 0 -- Accumulator does not require getAddr
getAddr IMMEDIATE = do
    addr <- getPC 
    setPC (addr + 1)
    return addr
getAddr ZEROPAGE = do
    cPC <- getPC
    lb <- readByte cPC -- Get the low byte
    let addr = joinBytes 0x00 lb
    setPC (cPC + 1)
    return addr
getAddr ZEROPAGE_X = do
    cPC <- getPC
    xreg <- getIDX
    lb <- readByte cPC -- Get the low byte of the address
    let addr = joinBytes 0x00 (lb + xreg) -- Add IDX ro the address
    setPC (cPC + 1)
    return addr
getAddr ZEROPAGE_Y = do
    cPC <- getPC
    yreg <- getIDY
    lb <- readByte cPC -- Get the low byte of the address
    let addr = joinBytes 0x00 (lb + yreg) -- Add IDY to the address
    setPC (cPC + 1)
    return addr
getAddr RELATIVE = do
    cPC <- getPC
    offset <- readByte cPC -- Get the offset byte
    let iPC = fromIntegral cPC :: Int
    let iOffset = fromIntegral (fromIntegral offset :: Int8) :: Int
    let addr = fromIntegral (iPC + iOffset) + 1 :: Word16
    setPC (cPC + 1)
    return addr
getAddr ABSOLUTE = do
    cPC <- getPC
    lb <- readByte cPC -- Load the low byte of the address location
    hb <- readByte (cPC + 1) -- Load the high byte of the address location
    let addr = joinBytes hb lb
    setPC (cPC + 2)
    return addr
getAddr ABSOLUTE_X = do
    cPC <- getPC
    xreg <- getIDX
    lb <- readByte cPC -- Load the low byte of the address location
    hb <- readByte (cPC + 1) -- Load the high byte of the address location
    let addr = (joinBytes hb lb) + (joinBytes 0x00 xreg) -- Add the resulting 2-byte with the IDX register
    setPC (cPC + 2)
    return addr
getAddr ABSOLUTE_Y = do
    cPC <- getPC
    yreg <- getIDY
    lb <- readByte cPC -- Load the low byte of the address location
    hb <- readByte (cPC + 1) -- Load the high byte of the address location
    let addr = (joinBytes hb lb) + (joinBytes 0x00 yreg) -- Add the resulting 2-byte with the IDY register
    setPC (cPC + 2)
    return addr
getAddr INDIRECT = do
    cPC <- getPC
    let (pchb, pclb) = splitBytes cPC

    -- There is a BUG in the MOS 6502 where, if the indirect vector falls on a page boundary (i.e. 0xXXFF) then the LSB is fetched correctly from the address 0xXXFF
    -- But the MSB is taken incorrectly from 0xXX00.
    -- That is going to be emulated by adding fetching the high byte from (hb) (lb + 1) where overflow may happen in the addition.
    addr_lb <- readByte (joinBytes pchb pclb) -- Load the low byte of the address location
    addr_hb <- readByte ((joinBytes pchb pclb) + 1) -- Load the high byte of the address location
    let addr1 = joinBytes addr_hb addr_lb
    lb <- readByte addr1 -- Get the actual address from that location
    let (hb1, lb1) = splitBytes addr1
    hb <- readByte (joinBytes (hb1) (lb1 + 1))
    let addr = joinBytes hb lb
    setPC (cPC + 2)
    return addr
getAddr INDIRECT_X = do
    cPC <- getPC
    table_start <- readByte cPC -- PC Holds the start address of a memory table
    table_offset <- getIDX
    let table_addr = table_start + table_offset -- Get table_addr = table_start + offset (Word8)
    lb <- readByte (joinBytes 0x00 table_addr) -- Read low byte
    hb <- readByte (joinBytes 0x00 (table_addr + 1)) -- Read high byte
    let addr = joinBytes hb lb
    setPC (cPC + 1)
    return addr
getAddr INDIRECT_Y = do
    cPC <- getPC 
    table_lb <- readByte cPC -- PC contains the zero-page memory address which contains the low byte of the actual output
    addr_lb <- readByte $ joinBytes 0x00 table_lb
    addr_hb <- readByte $ joinBytes 0x00 (table_lb + 1)
    let addr = joinBytes addr_hb addr_lb
    yreg <- getIDY
    let faddr = addr + (joinBytes 0x00 yreg)
    setPC (cPC + 1)
    return faddr


writeStack :: Word8 -> StateT MOS6502 IO ()
writeStack byte = do
    sp <- getSP
    let addr = 0x0100 + (joinBytes 0x00 sp) -- Stack is between 0x0100 and 0x01FF
    writeByte addr byte -- Write byte to stack
    mapSP (\x -> x - 1) -- Decrement stack pointer


readStack :: StateT MOS6502 IO Word8
readStack = do
    mapSP (+ 1) -- Increment stack pointer
    sp <- getSP -- Get the Stack Pointer
    let addr = 0x0100 + (joinBytes 0x00 sp) -- Stack is between 0x0100 and 0x01FF
    readByte addr -- Read byte


getCycles :: StateT MOS6502 IO Int
getCycles = cycles <$> get


incClock :: StateT MOS6502 IO ()
incClock = modify (\mos -> mos{clock = 1 + (clock mos)})


resetCycles :: StateT MOS6502 IO ()
resetCycles = modify (\mos -> mos{cycles = 0})


resetClock :: StateT MOS6502 IO ()
resetClock = modify (\mos -> mos{clock = 0})


updateCycles :: Int -> StateT MOS6502 IO ()
updateCycles offset = modify (\mos -> mos{cycles = offset + cycles mos})


setComplete :: Bool -> StateT MOS6502 IO ()
setComplete b = modify (\mos -> mos{context = (context mos){complete = b}})


fetchComplete :: StateT MOS6502 IO Bool
fetchComplete = do
    mos <- get
    let c = complete . context $ mos
    setComplete False
    return c


tick :: StateT MOS6502 IO ()
tick = do
    incClock
    c <- getCycles
    if c > 0
        then do
            updateCycles (-1)
        else do
            opcode <- fetch
            execute opcode
            setComplete True

tick' :: StateT MOS6502 IO Bool
tick' = do
    incClock
    c <- getCycles
    if c > 0
        then do
            updateCycles (-1)
            return False
        else do
            opcode <- fetch
            execute opcode
            return True


fetch :: StateT MOS6502 IO Word8
fetch = do
    pc <- getPC
    opcode <- readByte pc
    setPC (pc + 1)
    return opcode



execute :: Word8 -> StateT MOS6502 IO ()
execute 0x69 = do
    updateCycles 2
    opADC IMMEDIATE
execute 0x65 = do
    updateCycles 3
    opADC ZEROPAGE
execute 0x75 = do
    updateCycles 4
    opADC ZEROPAGE_X
execute 0x6D = do
    updateCycles 4
    opADC ABSOLUTE
execute 0x7D = do
    updateCycles 4
    opADC ABSOLUTE_X
execute 0x79 = do
    updateCycles 4
    opADC ABSOLUTE_Y
execute 0x61 = do
    updateCycles 6
    opADC INDIRECT_X
execute 0x71 = do
    updateCycles 5
    opADC INDIRECT_Y
execute 0x29 = do
    updateCycles 2
    opAND IMMEDIATE
execute 0x25 = do
    updateCycles 3
    opAND ZEROPAGE
execute 0x35 = do
    updateCycles 4
    opAND ZEROPAGE_X
execute 0x2D = do
    updateCycles 4
    opAND ABSOLUTE
execute 0x3D = do
    updateCycles 4
    opAND ABSOLUTE_X
execute 0x39 = do
    updateCycles 4
    opAND ABSOLUTE_Y
execute 0x21 = do
    updateCycles 6
    opAND INDIRECT_X
execute 0x31 = do
    updateCycles 5
    opAND INDIRECT_Y
execute 0x0A = do
    updateCycles 2
    opASL ACCUMULATOR
execute 0x06 = do
    updateCycles 5
    opASL ZEROPAGE
execute 0x16 = do
    updateCycles 6
    opASL ZEROPAGE_X
execute 0x0E = do
    updateCycles 6
    opASL ABSOLUTE
execute 0x1E = do
    updateCycles 7
    opASL ABSOLUTE_X
execute 0x90 = do
    updateCycles 2
    opBCC RELATIVE
execute 0xB0 = do
    updateCycles 2
    opBCS RELATIVE
execute 0xF0 = do
    updateCycles 2
    opBEQ RELATIVE
execute 0x24 = do
    updateCycles 3
    opBIT ZEROPAGE
execute 0x2C = do
    updateCycles 4
    opBIT ABSOLUTE
execute 0x30 = do
    updateCycles 2
    opBMI RELATIVE
execute 0xD0 = do
    updateCycles 2
    opBNE RELATIVE
execute 0x10 = do
    updateCycles 2
    opBPL RELATIVE
execute 0x00 = do
    updateCycles 7
    opBRK IMPLICIT
execute 0x50 = do
    updateCycles 2
    opBVC RELATIVE
execute 0x70 = do
    updateCycles 2
    opBVS RELATIVE
execute 0x18 = do
    updateCycles 2
    opCLC IMPLICIT
execute 0xD8 = do
    updateCycles 2
    opCLD IMPLICIT
execute 0x58 = do
    updateCycles 2
    opCLI IMPLICIT
execute 0xB8 = do
    updateCycles 2
    opCLV IMPLICIT
execute 0xC9 = do
    updateCycles 2
    opCMP IMMEDIATE
execute 0xC5 = do
    updateCycles 3
    opCMP ZEROPAGE
execute 0xD5 = do
    updateCycles 4
    opCMP ZEROPAGE_X
execute 0xCD = do
    updateCycles 4
    opCMP ABSOLUTE
execute 0xDD = do
    updateCycles 4
    opCMP ABSOLUTE_X
execute 0xD9 = do
    updateCycles 4
    opCMP ABSOLUTE_Y
execute 0xC1 = do
    updateCycles 6
    opCMP INDIRECT_X
execute 0xD1 = do
    updateCycles 5
    opCMP INDIRECT_Y
execute 0xE0 = do
    updateCycles 2
    opCPX IMMEDIATE
execute 0xE4 = do
    updateCycles 3
    opCPX ZEROPAGE
execute 0xEC = do
    updateCycles 4
    opCPX ABSOLUTE
execute 0xC0 = do
    updateCycles 2
    opCPY IMMEDIATE
execute 0xC4 = do
    updateCycles 3
    opCPY ZEROPAGE
execute 0xCC = do
    updateCycles 4
    opCPY ABSOLUTE
execute 0xC6 = do
    updateCycles 5
    opDEC ZEROPAGE
execute 0xD6 = do
    updateCycles 6
    opDEC ZEROPAGE_X
execute 0xCE = do
    updateCycles 6
    opDEC ABSOLUTE
execute 0xDE = do
    updateCycles 7
    opDEC ABSOLUTE_X
execute 0xCA = do
    updateCycles 2
    opDEX IMPLICIT
execute 0x88 = do
    updateCycles 2
    opDEY IMPLICIT
execute 0x49 = do
    updateCycles 2
    opEOR IMMEDIATE
execute 0x45 = do
    updateCycles 3
    opEOR ZEROPAGE
execute 0x55 = do
    updateCycles 4
    opEOR ZEROPAGE_X
execute 0x4D = do
    updateCycles 4
    opEOR ABSOLUTE
execute 0x5D = do
    updateCycles 4
    opEOR ABSOLUTE_X
execute 0x59 = do
    updateCycles 4
    opEOR ABSOLUTE_Y
execute 0x41 = do
    updateCycles 6
    opEOR INDIRECT_X
execute 0x51 = do
    updateCycles 5
    opEOR INDIRECT_Y
execute 0xE6 = do
    updateCycles 5
    opINC ZEROPAGE
execute 0xF6 = do
    updateCycles 6
    opINC ZEROPAGE_X
execute 0xEE = do
    updateCycles 6
    opINC ABSOLUTE
execute 0xFE = do
    updateCycles 7
    opINC ABSOLUTE_X
execute 0xE8 = do
    updateCycles 2
    opINX IMPLICIT
execute 0xC8 = do
    updateCycles 2
    opINY IMPLICIT
execute 0x4C = do
    updateCycles 3
    opJMP ABSOLUTE
execute 0x6C = do
    updateCycles 5
    opJMP INDIRECT
execute 0x20 = do
    updateCycles 6
    opJSR ABSOLUTE
execute 0xA9 = do
    updateCycles 2
    opLDA IMMEDIATE
execute 0xA5 = do
    updateCycles 3
    opLDA ZEROPAGE
execute 0xB5 = do
    updateCycles 4
    opLDA ZEROPAGE_X
execute 0xAD = do
    updateCycles 4
    opLDA ABSOLUTE
execute 0xBD = do
    updateCycles 4
    opLDA ABSOLUTE_X
execute 0xB9 = do
    updateCycles 4
    opLDA ABSOLUTE_Y
execute 0xA1 = do
    updateCycles 6
    opLDA INDIRECT_X
execute 0xB1 = do
    updateCycles 5
    opLDA INDIRECT_Y
execute 0xA2 = do
    updateCycles 2
    opLDX IMMEDIATE
execute 0xA6 = do
    updateCycles 3
    opLDX ZEROPAGE
execute 0xB6 = do
    updateCycles 4
    opLDX ZEROPAGE_Y
execute 0xAE = do
    updateCycles 4
    opLDX ABSOLUTE
execute 0xBE = do
    updateCycles 4
    opLDX ABSOLUTE_Y
execute 0xA0 = do
    updateCycles 2
    opLDY IMMEDIATE
execute 0xA4 = do
    updateCycles 3
    opLDY ZEROPAGE
execute 0xB4 = do
    updateCycles 4
    opLDY ZEROPAGE_X
execute 0xAC = do
    updateCycles 4
    opLDY ABSOLUTE
execute 0xBC = do
    updateCycles 4
    opLDY ABSOLUTE_X
execute 0x4A = do
    updateCycles 2
    opLSR ACCUMULATOR
execute 0x46 = do
    updateCycles 5
    opLSR ZEROPAGE
execute 0x56 = do
    updateCycles 6
    opLSR ZEROPAGE_X
execute 0x4E = do
    updateCycles 6
    opLSR ABSOLUTE
execute 0x5E = do
    updateCycles 7
    opLSR ABSOLUTE_X
execute 0xEA = do
    updateCycles 2
    opNOP IMPLICIT
execute 0x09 = do
    updateCycles 2
    opORA IMMEDIATE
execute 0x05 = do
    updateCycles 3
    opORA ZEROPAGE
execute 0x15 = do
    updateCycles 4
    opORA ZEROPAGE_X
execute 0x0D = do
    updateCycles 4
    opORA ABSOLUTE
execute 0x1D = do
    updateCycles 4
    opORA ABSOLUTE_X
execute 0x19 = do
    updateCycles 4
    opORA ABSOLUTE_Y
execute 0x01 = do
    updateCycles 6
    opORA INDIRECT_X
execute 0x11 = do
    updateCycles 5
    opORA INDIRECT_Y
execute 0x48 = do
    updateCycles 3
    opPHA IMPLICIT
execute 0x08 = do
    updateCycles 3
    opPHP IMPLICIT
execute 0x68 = do
    updateCycles 4
    opPLA IMPLICIT
execute 0x28 = do
    updateCycles 4
    opPLP IMPLICIT
execute 0x2A = do
    updateCycles 2
    opROL ACCUMULATOR
execute 0x26 = do
    updateCycles 5
    opROL ZEROPAGE
execute 0x36 = do
    updateCycles 6
    opROL ZEROPAGE_X
execute 0x2E = do
    updateCycles 6
    opROL ABSOLUTE
execute 0x3E = do
    updateCycles 7
    opROL ABSOLUTE_X
execute 0x6A = do
    updateCycles 2
    opROR ACCUMULATOR
execute 0x66 = do
    updateCycles 5
    opROR ZEROPAGE
execute 0x76 = do
    updateCycles 6
    opROR ZEROPAGE_X
execute 0x6E = do
    updateCycles 6
    opROR ABSOLUTE
execute 0x7E = do
    updateCycles 7
    opROR ABSOLUTE_X
execute 0x40 = do
    updateCycles 6
    opRTI IMPLICIT
execute 0x60 = do
    updateCycles 6
    opRTS IMPLICIT
execute 0xE9 = do
    updateCycles 2
    opSBC IMMEDIATE
execute 0xE5 = do
    updateCycles 3
    opSBC ZEROPAGE
execute 0xF5 = do
    updateCycles 5
    opSBC ZEROPAGE_X
execute 0xED = do
    updateCycles 4
    opSBC ABSOLUTE
execute 0xFD = do
    updateCycles 4
    opSBC ABSOLUTE_X
execute 0xF9 = do
    updateCycles 4
    opSBC ABSOLUTE_Y
execute 0xE1 = do
    updateCycles 6
    opSBC INDIRECT_X
execute 0xF1 = do
    updateCycles 5
    opSBC INDIRECT_Y
execute 0x38 = do
    updateCycles 2
    opSEC IMPLICIT
execute 0xF8 = do
    updateCycles 2
    opSED IMPLICIT
execute 0x78 = do
    updateCycles 2
    opSEI IMPLICIT
execute 0x85 = do
    updateCycles 3
    opSTA ZEROPAGE
execute 0x95 = do
    updateCycles 4
    opSTA ZEROPAGE_X
execute 0x8D = do
    updateCycles 4
    opSTA ABSOLUTE
execute 0x9D = do
    updateCycles 5
    opSTA ABSOLUTE_X
execute 0x99 = do
    updateCycles 5
    opSTA ABSOLUTE_Y
execute 0x81 = do
    updateCycles 6
    opSTA INDIRECT_X
execute 0x91 = do
    updateCycles 6
    opSTA INDIRECT_Y
execute 0x86 = do
    updateCycles 3
    opSTX ZEROPAGE
execute 0x96 = do
    updateCycles 4
    opSTX ZEROPAGE_Y
execute 0x8E = do
    updateCycles 4
    opSTX ABSOLUTE
execute 0x84 = do
    updateCycles 3
    opSTY ZEROPAGE
execute 0x94 = do
    updateCycles 4
    opSTY ZEROPAGE_X
execute 0x8C = do
    updateCycles 4
    opSTY ABSOLUTE
execute 0xAA = do
    updateCycles 2
    opTAX IMPLICIT
execute 0xA8 = do
    updateCycles 2
    opTAY IMPLICIT
execute 0xBA = do
    updateCycles 2
    opTSX IMPLICIT
execute 0x8A = do
    updateCycles 2
    opTXA IMPLICIT
execute 0x9A = do
    updateCycles 2
    opTXS IMPLICIT
execute 0x98 = do
    updateCycles 2
    opTYA IMPLICIT
execute opcode = return ()
--error (show opcode ++ ": Unknown opcode") -- TODO: Add error log to Context perhaps?

iIRQ :: StateT MOS6502 IO ()
iIRQ = do
    interrupt_disable <- getFlag INTERRUPT_DISABLE
    if interrupt_disable
        then do
            return ()
        else do
            pc <- getPC
            let pushed_pc = pc + 1 -- Currently, PC points to the byte NEXT to the BRK instruction. But for some ill reason, the 6502 will push the byte after that one to the stack instead.
            let (pchb, pclb) = splitBytes (pushed_pc) --
            ps <- getPS
            writeStack pchb -- Write the high byte of the PC to the stack
            writeStack pclb -- Write the low byte of the PC to the stack
            writeStack (setBit ps 0) -- Write the PS to the stack with fourth bit (B flag) unset. (see: https://www.pagetable.com/?p=410)
            irq_lb <- readByte 0xFFFE -- Get the IRQ interrupt vector
            irq_hb <- readByte 0xFFFF --
            let jmp_addr = joinBytes irq_hb irq_lb
            setPC jmp_addr -- Jump to the address
            setFlag INTERRUPT_DISABLE True -- I'm not confident this happens. TODO: Verify this.

iNMI :: StateT MOS6502 IO () -- Non-Maskable Interrupt
iNMI = do
    pc <- getPC
    let pushed_pc = pc + 1 -- Currently, PC points to the byte NEXT to the BRK instruction. But for some ill reason, the 6502 will push the byte after that one to the stack instead.
    let (pchb, pclb) = splitBytes (pushed_pc) --
    ps <- getPS
    writeStack pchb -- Write the high byte of the PC to the stack
    writeStack pclb -- Write the low byte of the PC to the stack
    writeStack (setBit ps 0) -- Write the PS to the stack with fourth bit (B flag) unset. (see: https://www.pagetable.com/?p=410)
    irq_lb <- readByte 0xFFFA -- Get the NMI interrupt vector
    irq_hb <- readByte 0xFFFB --
    let jmp_addr = joinBytes irq_hb irq_lb
    setPC jmp_addr -- Jump to the address
    setFlag INTERRUPT_DISABLE True -- I'm not confident this happens. TODO: Verify this.

reset ::StateT MOS6502 IO () -- Non-Maskable Interrupt
reset = do
    irq_lb <- readByte 0xFFFC -- Get the NMI interrupt vector
    irq_hb <- readByte 0xFFFD --
    let jmp_addr = joinBytes irq_hb irq_lb
    setPC  jmp_addr -- Set the PC Register
    setSP  0xFD
    setACC 0x00
    setIDX 0x00
    setIDY 0x00
    setPS (setBit 0x00 5 :: Word8) -- Set the Status Flag. (UNUSED flag set to 1)
    resetClock
    resetCycles

-- TODO: Reset CPU Context

-- INSTRUCTIONS:
--
-- opXXX IMPLICIT = error "Operation XXX does not support IMPLICIT addressing mode"
-- opXXX ACCUMULATOR = error "Operation XXX does not support ACCUMULATOR addressing mode"
-- opXXX IMMEDIATE = error "Operation XXX does not support IMMEDIATE addressing mode"
-- opXXX ZEROPAGE = error "Operation XXX does not support ZEROPAGE addressing mode"
-- opXXX ZEROPAGE_X = error "Operation XXX does not support ZEROPAGE_X addressing mode"
-- opXXX ZEROPAGE_Y = error "Operation XXX does not support ZEROPAGE_Y addressing mode"
-- opXXX RELATIVE = error "Operation XXX does not support RELATIVE addressing mode"
-- opXXX ABSOLUTE = error "Operation XXX does not support ABSOLUTE addressing mode"
-- opXXX ABSOLUTE_X = error "Operation XXX does not support ABSOLUTE_X addressing mode"
-- opXXX ABSOLUTE_Y = error "Operation XXX does not support ABSOLUTE_Y addressing mode"
-- opXXX INDIRECT = error "Operation XXX does not support INDIRECT addressing mode"
-- opXXX INDIRECT_X = error "Operation XXX does not support INDIRECT_X addressing mode"
-- opXXX INDIRECT_Y = error "Operation XXX does not support INDIRECT_Y addressing mode"
--
--
-- Note: Instructions do not have to update the PC unless they use additional operands through the means of addressing modes.
-- The PC should be updated before the call to the instruction.

opADC ::ADDR_MODE -> StateT MOS6502 IO ()
opADC IMPLICIT = error "Operation ADC does not support IMPLICIT addressing mode"
opADC ACCUMULATOR = error "Operation ADC does not support ACCUMULATOR addressing mode"
opADC ZEROPAGE_Y = error "Operation ADC does not support ZEROPAGE_Y addressing mode"
opADC RELATIVE = error "Operation ADC does not support RELATIVE addressing mode"
opADC INDIRECT = error "Operation ADC does not support INDIRECT addressing mode"
opADC addr_mode = do
    acc <- getACC 
    carry_flag <- getFlag $ CARRY -- Get the Accumulator registers prior to changes
    let carry = if carry_flag then 1 else 0 :: Word8
    decimal_flag <- getFlag $ DECIMAL_MODE
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    let iacc = fromIntegral acc :: Int
    let ibyte = fromIntegral byte :: Int
    let icarry = fromIntegral carry :: Int
    let result = iacc + ibyte + icarry
    setFlag ZERO ((result .&. 0xFF) == 0) -- Sets the Zero flag if the result is equal to 0
    if decimal_flag
        then do
            let ln = (fromIntegral acc .&. 0xF) + (fromIntegral byte .&. 0xF) + fromIntegral carry :: Word16
            let ln' = if (ln >= 0xA) then ((ln + 0x6) .&. 0xF) + 0x10 else ln
            let r = (fromIntegral acc .&. 0xF0) + (fromIntegral byte .&. 0xF0) + ln' :: Word16
            setFlag NEGATIVE (b7 r)
            -- setFlag OVERFLOW (not (b7 (iacc `xor` ibyte)) && (b7 (iacc `xor` result)))
            setFlag OVERFLOW (((((r `xor` fromIntegral acc) .&. (r `xor` fromIntegral byte)) .&. 0x80) .>>. 1) /= 0)
            let r' = if r >= 0xA0 then r + 0x60 else r
            setFlag CARRY (r' .>>. 8 /= 0)
            let acc' = fromIntegral r' :: Word8
            setACC acc'
        else do
            setFlag NEGATIVE (b7 result)
            setFlag OVERFLOW (not (b7 (iacc `xor` ibyte)) && (b7 (iacc `xor` result)))
            setFlag CARRY (result > 0xFF)
            let acc' = fromIntegral (result .&. 0xFF) :: Word8
            setACC acc'

opAND ::ADDR_MODE -> StateT MOS6502 IO ()
opAND IMPLICIT = error "Operation AND does not support IMPLICIT addressing mode"
opAND ACCUMULATOR = error "Operation AND does not support ACCUMULATOR addressing mode"
opAND ZEROPAGE_Y = error "Operation AND does not support ZEROPAGE_Y addressing mode"
opAND RELATIVE = error "Operation AND does not support RELATIVE addressing mode"
opAND INDIRECT = error "Operation AND does not support INDIRECT addressing mode"
opAND addr_mode = do
    old_acc <- getACC 
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    mapACC (.&. byte) -- AND the corresponding byte to Accumulator
    acc <- getACC 
    setFlag ZERO (acc == 0) -- Sets the Zero flag if the result is equal to 0
    setFlag NEGATIVE (b7 acc) -- Sets the Negative flag is the result is negative

opASL ::ADDR_MODE -> StateT MOS6502 IO ()
opASL IMPLICIT = error "Operation ASL does not support IMPLICIT addressing mode"
opASL IMMEDIATE = error "Operation ASL does not support IMMEDIATE addressing mode"
opASL ZEROPAGE_Y = error "Operation ASL does not support ZEROPAGE_Y addressing mode"
opASL RELATIVE = error "Operation ASL does not support RELATIVE addressing mode"
opASL ABSOLUTE_Y = error "Operation ASL does not support ABSOLUTE_Y addressing mode"
opASL INDIRECT = error "Operation ASL does not support INDIRECT addressing mode"
opASL INDIRECT_X = error "Operation ASL does not support INDIRECT_X addressing mode"
opASL INDIRECT_Y = error "Operation ASL does not support INDIRECT_Y addressing mode"
opASL ACCUMULATOR = do
    old_acc <- getACC 
    let carry_flag = b7 old_acc -- Carry flag is set to contents of old bit 7
    mapACC ((\x -> x .<<. 1) :: Word8 -> Word8) -- Shifts byte one bit to the left
    acc <- getACC 
    setFlag CARRY carry_flag -- Sets the Carry flag
    setFlag ZERO (acc == 0) -- Sets the Zero flag if the result is equal to 0
    setFlag NEGATIVE (b7 acc) -- Sets the Negative flag is the result is negative
opASL addr_mode = do
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    let carry_flag = b7 byte -- Carry flag is set to contents of old bit 7
    let new_byte = byte .<<. 1 :: Word8 -- Perform the L Shift
    writeByte addr new_byte -- Write new byte to same address
    setFlag CARRY carry_flag -- Sets the Carry flag
    setFlag ZERO (new_byte == 0) -- Sets the Zero flag if the result is equal to 0
    setFlag NEGATIVE (b7 new_byte) -- Sets the Negative flag is the result is negative

opBCC ::ADDR_MODE -> StateT MOS6502 IO ()
opBCC IMPLICIT = error "Operation BCC does not support IMPLICIT addressing mode"
opBCC ACCUMULATOR = error "Operation BCC does not support ACCUMULATOR addressing mode"
opBCC IMMEDIATE = error "Operation BCC does not support IMMEDIATE addressing mode"
opBCC ZEROPAGE = error "Operation BCC does not support ZEROPAGE addressing mode"
opBCC ZEROPAGE_X = error "Operation BCC does not support ZEROPAGE_X addressing mode"
opBCC ZEROPAGE_Y = error "Operation BCC does not support ZEROPAGE_Y addressing mode"
opBCC ABSOLUTE = error "Operation BCC does not support ABSOLUTE addressing mode"
opBCC ABSOLUTE_X = error "Operation BCC does not support ABSOLUTE_X addressing mode"
opBCC ABSOLUTE_Y = error "Operation BCC does not support ABSOLUTE_Y addressing mode"
opBCC INDIRECT = error "Operation BCC does not support INDIRECT addressing mode"
opBCC INDIRECT_X = error "Operation BCC does not support INDIRECT_X addressing mode"
opBCC INDIRECT_Y = error "Operation BCC does not support INDIRECT_Y addressing mode"
opBCC RELATIVE = do
    carry_flag <- getFlag CARRY -- Get carry flag
    addr <- getAddr RELATIVE -- Get jump address
    setPCIf (not carry_flag) addr -- Jump if Carry flag is clear

opBCS ::ADDR_MODE -> StateT MOS6502 IO ()
opBCS IMPLICIT = error "Operation BCS does not support IMPLICIT addressing mode"
opBCS ACCUMULATOR = error "Operation BCS does not support ACCUMULATOR addressing mode"
opBCS IMMEDIATE = error "Operation BCS does not support IMMEDIATE addressing mode"
opBCS ZEROPAGE = error "Operation BCS does not support ZEROPAGE addressing mode"
opBCS ZEROPAGE_X = error "Operation BCS does not support ZEROPAGE_X addressing mode"
opBCS ZEROPAGE_Y = error "Operation BCS does not support ZEROPAGE_Y addressing mode"
opBCS ABSOLUTE = error "Operation BCS does not support ABSOLUTE addressing mode"
opBCS ABSOLUTE_X = error "Operation BCS does not support ABSOLUTE_X addressing mode"
opBCS ABSOLUTE_Y = error "Operation BCS does not support ABSOLUTE_Y addressing mode"
opBCS INDIRECT = error "Operation BCS does not support INDIRECT addressing mode"
opBCS INDIRECT_X = error "Operation BCS does not support INDIRECT_X addressing mode"
opBCS INDIRECT_Y = error "Operation BCS does not support INDIRECT_Y addressing mode"
opBCS RELATIVE = do
    carry_flag <- getFlag CARRY -- Get carry flag
    addr <- getAddr RELATIVE -- Get jump address
    setPCIf carry_flag addr -- Jump if Carry flag is set

opBEQ ::ADDR_MODE -> StateT MOS6502 IO ()
opBEQ IMPLICIT = error "Operation BEQ does not support IMPLICIT addressing mode"
opBEQ ACCUMULATOR = error "Operation BEQ does not support ACCUMULATOR addressing mode"
opBEQ IMMEDIATE = error "Operation BEQ does not support IMMEDIATE addressing mode"
opBEQ ZEROPAGE = error "Operation BEQ does not support ZEROPAGE addressing mode"
opBEQ ZEROPAGE_X = error "Operation BEQ does not support ZEROPAGE_X addressing mode"
opBEQ ZEROPAGE_Y = error "Operation BEQ does not support ZEROPAGE_Y addressing mode"
opBEQ ABSOLUTE = error "Operation BEQ does not support ABSOLUTE addressing mode"
opBEQ ABSOLUTE_X = error "Operation BEQ does not support ABSOLUTE_X addressing mode"
opBEQ ABSOLUTE_Y = error "Operation BEQ does not support ABSOLUTE_Y addressing mode"
opBEQ INDIRECT = error "Operation BEQ does not support INDIRECT addressing mode"
opBEQ INDIRECT_X = error "Operation BEQ does not support INDIRECT_X addressing mode"
opBEQ INDIRECT_Y = error "Operation BEQ does not support INDIRECT_Y addressing mode"
opBEQ RELATIVE = do
    zero_flag <- getFlag ZERO -- Get Zero flag
    addr <- getAddr RELATIVE -- Get jump address
    setPCIf zero_flag addr -- Jump if Zero flag is set

opBIT ::ADDR_MODE -> StateT MOS6502 IO ()
opBIT IMPLICIT = error "Operation BIT does not support IMPLICIT addressing mode"
opBIT ACCUMULATOR = error "Operation BIT does not support ACCUMULATOR addressing mode"
opBIT IMMEDIATE = error "Operation BIT does not support IMMEDIATE addressing mode"
opBIT ZEROPAGE_X = error "Operation BIT does not support ZEROPAGE_X addressing mode"
opBIT ZEROPAGE_Y = error "Operation BIT does not support ZEROPAGE_Y addressing mode"
opBIT RELATIVE = error "Operation BIT does not support RELATIVE addressing mode"
opBIT ABSOLUTE_X = error "Operation BIT does not support ABSOLUTE_X addressing mode"
opBIT ABSOLUTE_Y = error "Operation BIT does not support ABSOLUTE_Y addressing mode"
opBIT INDIRECT = error "Operation BIT does not support INDIRECT addressing mode"
opBIT INDIRECT_X = error "Operation BIT does not support INDIRECT_X addressing mode"
opBIT INDIRECT_Y = error "Operation BIT does not support INDIRECT_Y addressing mode"
opBIT addr_mode = do
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    acc <- getACC 
    let and_result = (byte .&. acc) -- Perform AND operation
    setFlag ZERO (and_result == 0) -- Sets the ZERO flag if the result of the AND operation is 0
    setFlag NEGATIVE (b7 byte) -- Sets the Negative flag to the seventh bit of the address value
    setFlag OVERFLOW (b6 byte) -- Sets the Overflow flag to the sixth bit of the address value

opBMI ::ADDR_MODE -> StateT MOS6502 IO ()
opBMI IMPLICIT = error "Operation BMI does not support IMPLICIT addressing mode"
opBMI ACCUMULATOR = error "Operation BMI does not support ACCUMULATOR addressing mode"
opBMI IMMEDIATE = error "Operation BMI does not support IMMEDIATE addressing mode"
opBMI ZEROPAGE = error "Operation BMI does not support ZEROPAGE addressing mode"
opBMI ZEROPAGE_X = error "Operation BMI does not support ZEROPAGE_X addressing mode"
opBMI ZEROPAGE_Y = error "Operation BMI does not support ZEROPAGE_Y addressing mode"
opBMI ABSOLUTE = error "Operation BMI does not support ABSOLUTE addressing mode"
opBMI ABSOLUTE_X = error "Operation BMI does not support ABSOLUTE_X addressing mode"
opBMI ABSOLUTE_Y = error "Operation BMI does not support ABSOLUTE_Y addressing mode"
opBMI INDIRECT = error "Operation BMI does not support INDIRECT addressing mode"
opBMI INDIRECT_X = error "Operation BMI does not support INDIRECT_X addressing mode"
opBMI INDIRECT_Y = error "Operation BMI does not support INDIRECT_Y addressing mode"
opBMI RELATIVE = do
    negative_flag <- getFlag NEGATIVE -- Get Negative flag
    addr <- getAddr RELATIVE -- Get jump address
    setPCIf negative_flag addr -- Jump if Negative flag is set

opBNE ::ADDR_MODE -> StateT MOS6502 IO ()
opBNE IMPLICIT = error "Operation BNE does not support IMPLICIT addressing mode"
opBNE ACCUMULATOR = error "Operation BNE does not support ACCUMULATOR addressing mode"
opBNE IMMEDIATE = error "Operation BNE does not support IMMEDIATE addressing mode"
opBNE ZEROPAGE = error "Operation BNE does not support ZEROPAGE addressing mode"
opBNE ZEROPAGE_X = error "Operation BNE does not support ZEROPAGE_X addressing mode"
opBNE ZEROPAGE_Y = error "Operation BNE does not support ZEROPAGE_Y addressing mode"
opBNE ABSOLUTE = error "Operation BNE does not support ABSOLUTE addressing mode"
opBNE ABSOLUTE_X = error "Operation BNE does not support ABSOLUTE_X addressing mode"
opBNE ABSOLUTE_Y = error "Operation BNE does not support ABSOLUTE_Y addressing mode"
opBNE INDIRECT = error "Operation BNE does not support INDIRECT addressing mode"
opBNE INDIRECT_X = error "Operation BNE does not support INDIRECT_X addressing mode"
opBNE INDIRECT_Y = error "Operation BNE does not support INDIRECT_Y addressing mode"
opBNE RELATIVE = do
    zero_flag <- getFlag ZERO -- Get Zero flag
    addr <- getAddr RELATIVE -- Get jump address
    setPCIf (not zero_flag) addr -- Jump if Zero flag is set

opBPL ::ADDR_MODE -> StateT MOS6502 IO ()
opBPL IMPLICIT = error "Operation BPL does not support IMPLICIT addressing mode"
opBPL ACCUMULATOR = error "Operation BPL does not support ACCUMULATOR addressing mode"
opBPL IMMEDIATE = error "Operation BPL does not support IMMEDIATE addressing mode"
opBPL ZEROPAGE = error "Operation BPL does not support ZEROPAGE addressing mode"
opBPL ZEROPAGE_X = error "Operation BPL does not support ZEROPAGE_X addressing mode"
opBPL ZEROPAGE_Y = error "Operation BPL does not support ZEROPAGE_Y addressing mode"
opBPL ABSOLUTE = error "Operation BPL does not support ABSOLUTE addressing mode"
opBPL ABSOLUTE_X = error "Operation BPL does not support ABSOLUTE_X addressing mode"
opBPL ABSOLUTE_Y = error "Operation BPL does not support ABSOLUTE_Y addressing mode"
opBPL INDIRECT = error "Operation BPL does not support INDIRECT addressing mode"
opBPL INDIRECT_X = error "Operation BPL does not support INDIRECT_X addressing mode"
opBPL INDIRECT_Y = error "Operation BPL does not support INDIRECT_Y addressing mode"
opBPL RELATIVE = do
    negative_flag <- getFlag NEGATIVE -- Get Zero flag
    addr <- getAddr RELATIVE -- Get jump address
    setPCIf (not negative_flag) addr -- Jump if Zero flag is set

opBRK ::ADDR_MODE -> StateT MOS6502 IO ()
opBRK ACCUMULATOR = error "Operation BRK does not support ACCUMULATOR addressing mode"
opBRK IMMEDIATE = error "Operation BRK does not support IMMEDIATE addressing mode"
opBRK ZEROPAGE = error "Operation BRK does not support ZEROPAGE addressing mode"
opBRK ZEROPAGE_X = error "Operation BRK does not support ZEROPAGE_X addressing mode"
opBRK ZEROPAGE_Y = error "Operation BRK does not support ZEROPAGE_Y addressing mode"
opBRK RELATIVE = error "Operation BRK does not support RELATIVE addressing mode"
opBRK ABSOLUTE = error "Operation BRK does not support ABSOLUTE addressing mode"
opBRK ABSOLUTE_X = error "Operation BRK does not support ABSOLUTE_X addressing mode"
opBRK ABSOLUTE_Y = error "Operation BRK does not support ABSOLUTE_Y addressing mode"
opBRK INDIRECT = error "Operation BRK does not support INDIRECT addressing mode"
opBRK INDIRECT_X = error "Operation BRK does not support INDIRECT_X addressing mode"
opBRK INDIRECT_Y = error "Operation BRK does not support INDIRECT_Y addressing mode"
opBRK IMPLICIT = do
    pc <- getPC 
    let pushed_pc = pc + 1 -- Currently, PC points to the byte NEXT to the BRK instruction. But for some ill reason, the 6502 will push the byte after that one to the stack instead.
    let (pchb, pclb) = splitBytes (pushed_pc) --
    ps <- getPS 
    writeStack pchb -- Write the high byte of the PC to the stack
    writeStack pclb -- Write the low byte of the PC to the stack
    writeStack (setBit ps 4) -- Write the PS to the stack with fourth bit (B flag) set.
    irq_lb <- readByte 0xFFFE -- Get the IRQ interrupt vector
    irq_hb <- readByte 0xFFFF --
    let jmp_addr = joinBytes irq_hb irq_lb
    setPC jmp_addr -- Jump to the address
    setFlag INTERRUPT_DISABLE True -- I'm not confident this happens. TODO: Verify this.

opBVC ::ADDR_MODE -> StateT MOS6502 IO ()
opBVC IMPLICIT = error "Operation BVC does not support IMPLICIT addressing mode"
opBVC ACCUMULATOR = error "Operation BVC does not support ACCUMULATOR addressing mode"
opBVC IMMEDIATE = error "Operation BVC does not support IMMEDIATE addressing mode"
opBVC ZEROPAGE = error "Operation BVC does not support ZEROPAGE addressing mode"
opBVC ZEROPAGE_X = error "Operation BVC does not support ZEROPAGE_X addressing mode"
opBVC ZEROPAGE_Y = error "Operation BVC does not support ZEROPAGE_Y addressing mode"
opBVC ABSOLUTE = error "Operation BVC does not support ABSOLUTE addressing mode"
opBVC ABSOLUTE_X = error "Operation BVC does not support ABSOLUTE_X addressing mode"
opBVC ABSOLUTE_Y = error "Operation BVC does not support ABSOLUTE_Y addressing mode"
opBVC INDIRECT = error "Operation BVC does not support INDIRECT addressing mode"
opBVC INDIRECT_X = error "Operation BVC does not support INDIRECT_X addressing mode"
opBVC INDIRECT_Y = error "Operation BVC does not support INDIRECT_Y addressing mode"
opBVC RELATIVE = do
    overflow_flag <- getFlag OVERFLOW -- Get Overflow flag
    addr <- getAddr RELATIVE -- Get jump address
    setPCIf (not overflow_flag) addr -- Jump if Zero flag is set

opBVS ::ADDR_MODE -> StateT MOS6502 IO ()
opBVS IMPLICIT = error "Operation BVS does not support IMPLICIT addressing mode"
opBVS ACCUMULATOR = error "Operation BVS does not support ACCUMULATOR addressing mode"
opBVS IMMEDIATE = error "Operation BVS does not support IMMEDIATE addressing mode"
opBVS ZEROPAGE = error "Operation BVS does not support ZEROPAGE addressing mode"
opBVS ZEROPAGE_X = error "Operation BVS does not support ZEROPAGE_X addressing mode"
opBVS ZEROPAGE_Y = error "Operation BVS does not support ZEROPAGE_Y addressing mode"
opBVS ABSOLUTE = error "Operation BVS does not support ABSOLUTE addressing mode"
opBVS ABSOLUTE_X = error "Operation BVS does not support ABSOLUTE_X addressing mode"
opBVS ABSOLUTE_Y = error "Operation BVS does not support ABSOLUTE_Y addressing mode"
opBVS INDIRECT = error "Operation BVS does not support INDIRECT addressing mode"
opBVS INDIRECT_X = error "Operation BVS does not support INDIRECT_X addressing mode"
opBVS INDIRECT_Y = error "Operation BVS does not support INDIRECT_Y addressing mode"
opBVS RELATIVE = do
    overflow_flag <- getFlag OVERFLOW -- Get Overflow flag
    addr <- getAddr RELATIVE -- Get jump address
    setPCIf overflow_flag addr -- Jump if Zero flag is set

opCLC ::ADDR_MODE -> StateT MOS6502 IO ()
opCLC ACCUMULATOR = error "Operation CLC does not support ACCUMULATOR addressing mode"
opCLC IMMEDIATE = error "Operation CLC does not support IMMEDIATE addressing mode"
opCLC ZEROPAGE = error "Operation CLC does not support ZEROPAGE addressing mode"
opCLC ZEROPAGE_X = error "Operation CLC does not support ZEROPAGE_X addressing mode"
opCLC ZEROPAGE_Y = error "Operation CLC does not support ZEROPAGE_Y addressing mode"
opCLC RELATIVE = error "Operation CLC does not support RELATIVE addressing mode"
opCLC ABSOLUTE = error "Operation CLC does not support ABSOLUTE addressing mode"
opCLC ABSOLUTE_X = error "Operation CLC does not support ABSOLUTE_X addressing mode"
opCLC ABSOLUTE_Y = error "Operation CLC does not support ABSOLUTE_Y addressing mode"
opCLC INDIRECT = error "Operation CLC does not support INDIRECT addressing mode"
opCLC INDIRECT_X = error "Operation CLC does not support INDIRECT_X addressing mode"
opCLC INDIRECT_Y = error "Operation CLC does not support INDIRECT_Y addressing mode"
opCLC IMPLICIT = do
    setFlag CARRY False

opCLD ::ADDR_MODE -> StateT MOS6502 IO ()
opCLD ACCUMULATOR = error "Operation CLD does not support ACCUMULATOR addressing mode"
opCLD IMMEDIATE = error "Operation CLD does not support IMMEDIATE addressing mode"
opCLD ZEROPAGE = error "Operation CLD does not support ZEROPAGE addressing mode"
opCLD ZEROPAGE_X = error "Operation CLD does not support ZEROPAGE_X addressing mode"
opCLD ZEROPAGE_Y = error "Operation CLD does not support ZEROPAGE_Y addressing mode"
opCLD RELATIVE = error "Operation CLD does not support RELATIVE addressing mode"
opCLD ABSOLUTE = error "Operation CLD does not support ABSOLUTE addressing mode"
opCLD ABSOLUTE_X = error "Operation CLD does not support ABSOLUTE_X addressing mode"
opCLD ABSOLUTE_Y = error "Operation CLD does not support ABSOLUTE_Y addressing mode"
opCLD INDIRECT = error "Operation CLD does not support INDIRECT addressing mode"
opCLD INDIRECT_X = error "Operation CLD does not support INDIRECT_X addressing mode"
opCLD INDIRECT_Y = error "Operation CLD does not support INDIRECT_Y addressing mode"
opCLD IMPLICIT = do
    setFlag DECIMAL_MODE False

opCLI ::ADDR_MODE -> StateT MOS6502 IO ()
opCLI ACCUMULATOR = error "Operation CLI does not support ACCUMULATOR addressing mode"
opCLI IMMEDIATE = error "Operation CLI does not support IMMEDIATE addressing mode"
opCLI ZEROPAGE = error "Operation CLI does not support ZEROPAGE addressing mode"
opCLI ZEROPAGE_X = error "Operation CLI does not support ZEROPAGE_X addressing mode"
opCLI ZEROPAGE_Y = error "Operation CLI does not support ZEROPAGE_Y addressing mode"
opCLI RELATIVE = error "Operation CLI does not support RELATIVE addressing mode"
opCLI ABSOLUTE = error "Operation CLI does not support ABSOLUTE addressing mode"
opCLI ABSOLUTE_X = error "Operation CLI does not support ABSOLUTE_X addressing mode"
opCLI ABSOLUTE_Y = error "Operation CLI does not support ABSOLUTE_Y addressing mode"
opCLI INDIRECT = error "Operation CLI does not support INDIRECT addressing mode"
opCLI INDIRECT_X = error "Operation CLI does not support INDIRECT_X addressing mode"
opCLI INDIRECT_Y = error "Operation CLI does not support INDIRECT_Y addressing mode"
opCLI IMPLICIT = do
    setFlag INTERRUPT_DISABLE False

opCLV ::ADDR_MODE -> StateT MOS6502 IO ()
opCLV ACCUMULATOR = error "Operation CLV does not support ACCUMULATOR addressing mode"
opCLV IMMEDIATE = error "Operation CLV does not support IMMEDIATE addressing mode"
opCLV ZEROPAGE = error "Operation CLV does not support ZEROPAGE addressing mode"
opCLV ZEROPAGE_X = error "Operation CLV does not support ZEROPAGE_X addressing mode"
opCLV ZEROPAGE_Y = error "Operation CLV does not support ZEROPAGE_Y addressing mode"
opCLV RELATIVE = error "Operation CLV does not support RELATIVE addressing mode"
opCLV ABSOLUTE = error "Operation CLV does not support ABSOLUTE addressing mode"
opCLV ABSOLUTE_X = error "Operation CLV does not support ABSOLUTE_X addressing mode"
opCLV ABSOLUTE_Y = error "Operation CLV does not support ABSOLUTE_Y addressing mode"
opCLV INDIRECT = error "Operation CLV does not support INDIRECT addressing mode"
opCLV INDIRECT_X = error "Operation CLV does not support INDIRECT_X addressing mode"
opCLV INDIRECT_Y = error "Operation CLV does not support INDIRECT_Y addressing mode"
opCLV IMPLICIT = do
    setFlag OVERFLOW False

opCMP ::ADDR_MODE -> StateT MOS6502 IO ()
opCMP IMPLICIT = error "Operation CMP does not support IMPLICIT addressing mode"
opCMP ACCUMULATOR = error "Operation CMP does not support ACCUMULATOR addressing mode"
opCMP ZEROPAGE_Y = error "Operation CMP does not support ZEROPAGE_Y addressing mode"
opCMP RELATIVE = error "Operation CMP does not support RELATIVE addressing mode"
opCMP INDIRECT = error "Operation CMP does not support INDIRECT addressing mode"
opCMP addr_mode = do
    acc <- getACC 
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    let result = acc - byte -- Compares the accumulator with a memory value
    setFlag ZERO (acc == byte) -- Set the Zero flag if they are equal
    setFlag CARRY (acc >= byte) -- Set the Carry flag if Acc >= mem_value
    setFlag NEGATIVE (b7 result) -- Set the Negative flag if

opCPX ::ADDR_MODE -> StateT MOS6502 IO ()
opCPX IMPLICIT = error "Operation CPX does not support IMPLICIT addressing mode"
opCPX ACCUMULATOR = error "Operation CPX does not support ACCUMULATOR addressing mode"
opCPX ZEROPAGE_X = error "Operation CPX does not support ZEROPAGE_X addressing mode"
opCPX ZEROPAGE_Y = error "Operation CPX does not support ZEROPAGE_Y addressing mode"
opCPX RELATIVE = error "Operation CPX does not support RELATIVE addressing mode"
opCPX ABSOLUTE_X = error "Operation CPX does not support ABSOLUTE_X addressing mode"
opCPX ABSOLUTE_Y = error "Operation CPX does not support ABSOLUTE_Y addressing mode"
opCPX INDIRECT = error "Operation CPX does not support INDIRECT addressing mode"
opCPX INDIRECT_X = error "Operation CPX does not support INDIRECT_X addressing mode"
opCPX INDIRECT_Y = error "Operation CPX does not support INDIRECT_Y addressing mode"
opCPX addr_mode = do
    xreg <- getIDX 
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    let result = xreg - byte -- Compares the accumulator with a memory value
    setFlag ZERO (xreg == byte) -- Set the Zero flag if they are equal
    setFlag CARRY (xreg >= byte) -- Set the Carry flag if Acc >= mem_value
    setFlag NEGATIVE (b7 result) -- Set the Negative flag if

opCPY ::ADDR_MODE -> StateT MOS6502 IO ()
opCPY IMPLICIT = error "Operation CPY does not support IMPLICIT addressing mode"
opCPY ACCUMULATOR = error "Operation CPY does not support ACCUMULATOR addressing mode"
opCPY ZEROPAGE_X = error "Operation CPY does not support ZEROPAGE_X addressing mode"
opCPY ZEROPAGE_Y = error "Operation CPY does not support ZEROPAGE_Y addressing mode"
opCPY RELATIVE = error "Operation CPY does not support RELATIVE addressing mode"
opCPY ABSOLUTE_X = error "Operation CPY does not support ABSOLUTE_X addressing mode"
opCPY ABSOLUTE_Y = error "Operation CPY does not support ABSOLUTE_Y addressing mode"
opCPY INDIRECT = error "Operation CPY does not support INDIRECT addressing mode"
opCPY INDIRECT_X = error "Operation CPY does not support INDIRECT_X addressing mode"
opCPY INDIRECT_Y = error "Operation CPY does not support INDIRECT_Y addressing mode"
opCPY addr_mode = do
    yreg <- getIDY 
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    let result = yreg - byte -- Compares the accumulator with a memory value
    setFlag ZERO (yreg == byte) -- Set the Zero flag if they are equal
    setFlag CARRY (yreg >= byte) -- Set the Carry flag if Acc >= mem_value
    setFlag NEGATIVE (b7 result) -- Set the Negative flag if

opDEC ::ADDR_MODE -> StateT MOS6502 IO ()
opDEC IMPLICIT = error "Operation DEC does not support IMPLICIT addressing mode"
opDEC ACCUMULATOR = error "Operation DEC does not support ACCUMULATOR addressing mode"
opDEC IMMEDIATE = error "Operation DEC does not support IMMEDIATE addressing mode"
opDEC ZEROPAGE_Y = error "Operation DEC does not support ZEROPAGE_Y addressing mode"
opDEC RELATIVE = error "Operation DEC does not support RELATIVE addressing mode"
opDEC ABSOLUTE_Y = error "Operation DEC does not support ABSOLUTE_Y addressing mode"
opDEC INDIRECT = error "Operation DEC does not support INDIRECT addressing mode"
opDEC INDIRECT_X = error "Operation DEC does not support INDIRECT_X addressing mode"
opDEC INDIRECT_Y = error "Operation DEC does not support INDIRECT_Y addressing mode"
opDEC addr_mode = do
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    let result = byte - 1
    writeByte addr result
    setFlag ZERO (result == 0) -- Sets the Zero flag is the result is equal to 0
    setFlag NEGATIVE (b7 result) -- Sets the Negative flag is the result is negative

opDEX ::ADDR_MODE -> StateT MOS6502 IO ()
opDEX ACCUMULATOR = error "Operation DEX does not support ACCUMULATOR addressing mode"
opDEX IMMEDIATE = error "Operation DEX does not support IMMEDIATE addressing mode"
opDEX ZEROPAGE = error "Operation DEX does not support ZEROPAGE addressing mode"
opDEX ZEROPAGE_X = error "Operation DEX does not support ZEROPAGE_X addressing mode"
opDEX ZEROPAGE_Y = error "Operation DEX does not support ZEROPAGE_Y addressing mode"
opDEX RELATIVE = error "Operation DEX does not support RELATIVE addressing mode"
opDEX ABSOLUTE = error "Operation DEX does not support ABSOLUTE addressing mode"
opDEX ABSOLUTE_X = error "Operation DEX does not support ABSOLUTE_X addressing mode"
opDEX ABSOLUTE_Y = error "Operation DEX does not support ABSOLUTE_Y addressing mode"
opDEX INDIRECT = error "Operation DEX does not support INDIRECT addressing mode"
opDEX INDIRECT_X = error "Operation DEX does not support INDIRECT_X addressing mode"
opDEX INDIRECT_Y = error "Operation DEX does not support INDIRECT_Y addressing mode"
opDEX IMPLICIT = do
    mapIDX (\x -> x - (1 :: Word8)) -- Increases the X Register by one
    idx <- getIDX 
    setFlag ZERO (idx == 0) -- Sets the Zero flag is the result is equal to 0
    setFlag NEGATIVE (b7 idx) -- Sets the Negative flag is the result is negative

opDEY ::ADDR_MODE -> StateT MOS6502 IO ()
opDEY ACCUMULATOR = error "Operation DEY does not support ACCUMULATOR addressing mode"
opDEY IMMEDIATE = error "Operation DEY does not support IMMEDIATE addressing mode"
opDEY ZEROPAGE = error "Operation DEY does not support ZEROPAGE addressing mode"
opDEY ZEROPAGE_X = error "Operation DEY does not support ZEROPAGE_X addressing mode"
opDEY ZEROPAGE_Y = error "Operation DEY does not support ZEROPAGE_Y addressing mode"
opDEY RELATIVE = error "Operation DEY does not support RELATIVE addressing mode"
opDEY ABSOLUTE = error "Operation DEY does not support ABSOLUTE addressing mode"
opDEY ABSOLUTE_X = error "Operation DEY does not support ABSOLUTE_X addressing mode"
opDEY ABSOLUTE_Y = error "Operation DEY does not support ABSOLUTE_Y addressing mode"
opDEY INDIRECT = error "Operation DEY does not support INDIRECT addressing mode"
opDEY INDIRECT_X = error "Operation DEY does not support INDIRECT_X addressing mode"
opDEY INDIRECT_Y = error "Operation DEY does not support INDIRECT_Y addressing mode"
opDEY IMPLICIT = do
    mapIDY (\x -> x - (1 :: Word8)) -- Increases the X Register by one
    idy <- getIDY 
    setFlag ZERO (idy == 0) -- Sets the Zero flag is the result is equal to 0
    setFlag NEGATIVE (b7 idy) -- Sets the Negative flag is the result is negative

opEOR ::ADDR_MODE -> StateT MOS6502 IO ()
opEOR IMPLICIT = error "Operation EOR does not support IMPLICIT addressing mode"
opEOR ACCUMULATOR = error "Operation EOR does not support ACCUMULATOR addressing mode"
opEOR ZEROPAGE_Y = error "Operation EOR does not support ZEROPAGE_Y addressing mode"
opEOR RELATIVE = error "Operation EOR does not support RELATIVE addressing mode"
opEOR INDIRECT = error "Operation EOR does not support INDIRECT addressing mode"
opEOR addr_mode = do
    old_acc <- getACC 
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    mapACC (`xor` byte) -- XOR the corresponding byte to Accumulator
    acc <- getACC 
    setFlag ZERO (acc == 0) -- Sets the Zero flag if the result is equal to 0
    setFlag NEGATIVE (b7 acc) -- Sets the Negative flag is the result is negative

opINC ::ADDR_MODE -> StateT MOS6502 IO ()
opINC IMPLICIT = error "Operation INC does not support IMPLICIT addressing mode"
opINC ACCUMULATOR = error "Operation INC does not support ACCUMULATOR addressing mode"
opINC IMMEDIATE = error "Operation INC does not support IMMEDIATE addressing mode"
opINC ZEROPAGE_Y = error "Operation INC does not support ZEROPAGE_Y addressing mode"
opINC RELATIVE = error "Operation INC does not support RELATIVE addressing mode"
opINC ABSOLUTE_Y = error "Operation INC does not support ABSOLUTE_Y addressing mode"
opINC INDIRECT = error "Operation INC does not support INDIRECT addressing mode"
opINC INDIRECT_X = error "Operation INC does not support INDIRECT_X addressing mode"
opINC INDIRECT_Y = error "Operation INC does not support INDIRECT_Y addressing mode"
opINC addr_mode = do
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    let result = byte + 1
    writeByte addr result
    setFlag ZERO (result == 0) -- Sets the Zero flag is the result is equal to 0
    setFlag NEGATIVE (b7 result) -- Sets the Negative flag is the result is negative

opINX ::ADDR_MODE -> StateT MOS6502 IO ()
opINX ACCUMULATOR = error "Operation INX does not support ACCUMULATOR addressing mode"
opINX IMMEDIATE = error "Operation INX does not support IMMEDIATE addressing mode"
opINX ZEROPAGE = error "Operation INX does not support ZEROPAGE addressing mode"
opINX ZEROPAGE_X = error "Operation INX does not support ZEROPAGE_X addressing mode"
opINX ZEROPAGE_Y = error "Operation INX does not support ZEROPAGE_Y addressing mode"
opINX RELATIVE = error "Operation INX does not support RELATIVE addressing mode"
opINX ABSOLUTE = error "Operation INX does not support ABSOLUTE addressing mode"
opINX ABSOLUTE_X = error "Operation INX does not support ABSOLUTE_X addressing mode"
opINX ABSOLUTE_Y = error "Operation INX does not support ABSOLUTE_Y addressing mode"
opINX INDIRECT = error "Operation INX does not support INDIRECT addressing mode"
opINX INDIRECT_X = error "Operation INX does not support INDIRECT_X addressing mode"
opINX INDIRECT_Y = error "Operation INX does not support INDIRECT_Y addressing mode"
opINX IMPLICIT = do
    mapIDX (+ (1 :: Word8)) -- Increases the X Register by one
    idx <- getIDX 
    setFlag ZERO (idx == 0) -- Sets the Zero flag is the result is equal to 0
    setFlag NEGATIVE (b7 idx) -- Sets the Negative flag is the result is negative

opINY ::ADDR_MODE -> StateT MOS6502 IO ()
opINY ACCUMULATOR = error "Operation INY does not support ACCUMULATOR addressing mode"
opINY IMMEDIATE = error "Operation INY does not support IMMEDIATE addressing mode"
opINY ZEROPAGE = error "Operation INY does not support ZEROPAGE addressing mode"
opINY ZEROPAGE_X = error "Operation INY does not support ZEROPAGE_X addressing mode"
opINY ZEROPAGE_Y = error "Operation INY does not support ZEROPAGE_Y addressing mode"
opINY RELATIVE = error "Operation INY does not support RELATIVE addressing mode"
opINY ABSOLUTE = error "Operation INY does not support ABSOLUTE addressing mode"
opINY ABSOLUTE_X = error "Operation INY does not support ABSOLUTE_X addressing mode"
opINY ABSOLUTE_Y = error "Operation INY does not support ABSOLUTE_Y addressing mode"
opINY INDIRECT = error "Operation INY does not support INDIRECT addressing mode"
opINY INDIRECT_X = error "Operation INY does not support INDIRECT_X addressing mode"
opINY INDIRECT_Y = error "Operation INY does not support INDIRECT_Y addressing mode"
opINY IMPLICIT = do
    mapIDY (+ (1 :: Word8)) -- Increases the X Register by one
    idy <- getIDY 
    setFlag ZERO (idy == 0) -- Sets the Zero flag is the result is equal to 0
    setFlag NEGATIVE (b7 idy) -- Sets the Negative flag is the result is negative

opJMP ::ADDR_MODE -> StateT MOS6502 IO ()
opJMP IMPLICIT = error "Operation JMP does not support IMPLICIT addressing mode"
opJMP ACCUMULATOR = error "Operation JMP does not support ACCUMULATOR addressing mode"
opJMP IMMEDIATE = error "Operation JMP does not support IMMEDIATE addressing mode"
opJMP ZEROPAGE = error "Operation JMP does not support ZEROPAGE addressing mode"
opJMP ZEROPAGE_X = error "Operation JMP does not support ZEROPAGE_X addressing mode"
opJMP ZEROPAGE_Y = error "Operation JMP does not support ZEROPAGE_Y addressing mode"
opJMP RELATIVE = error "Operation JMP does not support RELATIVE addressing mode"
opJMP ABSOLUTE_X = error "Operation JMP does not support ABSOLUTE_X addressing mode"
opJMP ABSOLUTE_Y = error "Operation JMP does not support ABSOLUTE_Y addressing mode"
opJMP INDIRECT_X = error "Operation JMP does not support INDIRECT_X addressing mode"
opJMP INDIRECT_Y = error "Operation JMP does not support INDIRECT_Y addressing mode"
opJMP addr_mode = do
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    setPC addr

opJSR ::ADDR_MODE -> StateT MOS6502 IO ()
opJSR IMPLICIT = error "Operation JSR does not support IMPLICIT addressing mode"
opJSR ACCUMULATOR = error "Operation JSR does not support ACCUMULATOR addressing mode"
opJSR IMMEDIATE = error "Operation JSR does not support IMMEDIATE addressing mode"
opJSR ZEROPAGE = error "Operation JSR does not support ZEROPAGE addressing mode"
opJSR ZEROPAGE_X = error "Operation JSR does not support ZEROPAGE_X addressing mode"
opJSR ZEROPAGE_Y = error "Operation JSR does not support ZEROPAGE_Y addressing mode"
opJSR RELATIVE = error "Operation JSR does not support RELATIVE addressing mode"
opJSR ABSOLUTE_X = error "Operation JSR does not support ABSOLUTE_X addressing mode"
opJSR ABSOLUTE_Y = error "Operation JSR does not support ABSOLUTE_Y addressing mode"
opJSR INDIRECT = error "Operation JSR does not support INDIRECT addressing mode"
opJSR INDIRECT_X = error "Operation JSR does not support INDIRECT_X addressing mode"
opJSR INDIRECT_Y = error "Operation JSR does not support INDIRECT_Y addressing mode"
opJSR addr_mode = do
    -- There is a peculiarity in the MOS 6502 where JSR will
    -- instead of pushing the position of the next address to stack, it will
    -- push the location prior to that address instead.
    pc <- getPC 
    let (hb, lb) = splitBytes (pc + 1)
    writeStack hb
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    writeStack lb
    setPC addr

opLDA ::ADDR_MODE -> StateT MOS6502 IO ()
opLDA IMPLICIT = error "Operation LDA does not support IMPLICIT addressing mode"
opLDA ACCUMULATOR = error "Operation LDA does not support ACCUMULATOR addressing mode"
opLDA ZEROPAGE_Y = error "Operation LDA does not support ZEROPAGE_Y addressing mode"
opLDA RELATIVE = error "Operation LDA does not support RELATIVE addressing mode"
opLDA INDIRECT = error "Operation LDA does not support INDIRECT addressing mode"
opLDA addr_mode = do
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr
    setACC byte
    setFlag ZERO (byte == 0)
    setFlag NEGATIVE (b7 byte)

opLDX ::ADDR_MODE -> StateT MOS6502 IO ()
opLDX IMPLICIT = error "Operation LDX does not support IMPLICIT addressing mode"
opLDX ACCUMULATOR = error "Operation LDX does not support ACCUMULATOR addressing mode"
opLDX ZEROPAGE_X = error "Operation LDX does not support ZEROPAGE_X addressing mode"
opLDX RELATIVE = error "Operation LDX does not support RELATIVE addressing mode"
opLDX ABSOLUTE_X = error "Operation LDX does not support ABSOLUTE_X addressing mode"
opLDX INDIRECT = error "Operation LDX does not support INDIRECT addressing mode"
opLDX INDIRECT_X = error "Operation LDX does not support INDIRECT_X addressing mode"
opLDX INDIRECT_Y = error "Operation LDX does not support INDIRECT_Y addressing mode"
opLDX addr_mode = do
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr
    setIDX byte
    setFlag ZERO (byte == 0)
    setFlag NEGATIVE (b7 byte)

opLDY ::ADDR_MODE -> StateT MOS6502 IO ()
opLDY IMPLICIT = error "Operation LDY does not support IMPLICIT addressing mode"
opLDY ACCUMULATOR = error "Operation LDY does not support ACCUMULATOR addressing mode"
opLDY ZEROPAGE_Y = error "Operation LDY does not support ZEROPAGE_X addressing mode"
opLDY RELATIVE = error "Operation LDY does not support RELATIVE addressing mode"
opLDY ABSOLUTE_Y = error "Operation LDY does not support ABSOLUTE_X addressing mode"
opLDY INDIRECT = error "Operation LDY does not support INDIRECT addressing mode"
opLDY INDIRECT_X = error "Operation LDY does not support INDIRECT_X addressing mode"
opLDY INDIRECT_Y = error "Operation LDY does not support INDIRECT_Y addressing mode"
opLDY addr_mode = do
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr
    setIDY byte
    setFlag ZERO (byte == 0)
    setFlag NEGATIVE (b7 byte)

opLSR ::ADDR_MODE -> StateT MOS6502 IO ()
opLSR IMPLICIT = error "Operation LSR does not support IMPLICIT addressing mode"
opLSR IMMEDIATE = error "Operation LSR does not support IMMEDIATE addressing mode"
opLSR ZEROPAGE_Y = error "Operation LSR does not support ZEROPAGE_Y addressing mode"
opLSR RELATIVE = error "Operation LSR does not support RELATIVE addressing mode"
opLSR ABSOLUTE_Y = error "Operation LSR does not support ABSOLUTE_Y addressing mode"
opLSR INDIRECT = error "Operation LSR does not support INDIRECT addressing mode"
opLSR INDIRECT_X = error "Operation LSR does not support INDIRECT_X addressing mode"
opLSR INDIRECT_Y = error "Operation LSR does not support INDIRECT_Y addressing mode"
opLSR ACCUMULATOR = do
    old_acc <- getACC 
    let carry_flag = b0 old_acc -- Carry flag is set to contents of old bit 0
    mapACC ((\x -> x .>>. 1) :: Word8 -> Word8)
    acc <- getACC 
    setFlag CARRY carry_flag
    setFlag ZERO (acc == 0) -- Sets the Zero flag if the result is equal to 0
    setFlag NEGATIVE (b7 acc) -- Sets the Negative flag is the result is negative
opLSR addr_mode = do
    addr <- getAddr addr_mode
    byte <- readByte addr
    let carry_flag = b0 byte -- Carry flag is set to contents of old bit 0
    let new_byte = byte .>>. 1 :: Word8
    writeByte addr new_byte
    setFlag CARRY carry_flag
    setFlag ZERO (new_byte == 0) -- Sets the Zero flag if the result is equal to 0
    setFlag NEGATIVE (b7 new_byte) -- Sets the Negative flag is the result is negative

opNOP ::ADDR_MODE -> StateT MOS6502 IO ()
opNOP ACCUMULATOR = error "Operation NOP does not support ACCUMULATOR addressing mode"
opNOP IMMEDIATE = error "Operation NOP does not support IMMEDIATE addressing mode"
opNOP ZEROPAGE = error "Operation NOP does not support ZEROPAGE addressing mode"
opNOP ZEROPAGE_X = error "Operation NOP does not support ZEROPAGE_X addressing mode"
opNOP ZEROPAGE_Y = error "Operation NOP does not support ZEROPAGE_Y addressing mode"
opNOP RELATIVE = error "Operation NOP does not support RELATIVE addressing mode"
opNOP ABSOLUTE = error "Operation NOP does not support ABSOLUTE addressing mode"
opNOP ABSOLUTE_X = error "Operation NOP does not support ABSOLUTE_X addressing mode"
opNOP ABSOLUTE_Y = error "Operation NOP does not support ABSOLUTE_Y addressing mode"
opNOP INDIRECT = error "Operation NOP does not support INDIRECT addressing mode"
opNOP INDIRECT_X = error "Operation NOP does not support INDIRECT_X addressing mode"
opNOP INDIRECT_Y = error "Operation NOP does not support INDIRECT_Y addressing mode"
opNOP IMPLICIT = return ()

opORA ::ADDR_MODE -> StateT MOS6502 IO ()
opORA IMPLICIT = error "Operation ORA does not support IMPLICIT addressing mode"
opORA ACCUMULATOR = error "Operation ORA does not support ACCUMULATOR addressing mode"
opORA ZEROPAGE_Y = error "Operation ORA does not support ZEROPAGE_Y addressing mode"
opORA RELATIVE = error "Operation ORA does not support RELATIVE addressing mode"
opORA INDIRECT = error "Operation ORA does not support INDIRECT addressing mode"
opORA addr_mode = do
    old_acc <- getACC 
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    mapACC (.|. byte) -- OR the corresponding byte to Accumulator
    acc <- getACC 
    setFlag ZERO (acc == 0) -- Sets the Zero flag if the result is equal to 0
    setFlag NEGATIVE (b7 acc) -- Sets the Negative flag is the result is negative

opPHA ::ADDR_MODE -> StateT MOS6502 IO ()
opPHA ACCUMULATOR = error "Operation PHA does not support ACCUMULATOR addressing mode"
opPHA IMMEDIATE = error "Operation PHA does not support IMMEDIATE addressing mode"
opPHA ZEROPAGE = error "Operation PHA does not support ZEROPAGE addressing mode"
opPHA ZEROPAGE_X = error "Operation PHA does not support ZEROPAGE_X addressing mode"
opPHA ZEROPAGE_Y = error "Operation PHA does not support ZEROPAGE_Y addressing mode"
opPHA RELATIVE = error "Operation PHA does not support RELATIVE addressing mode"
opPHA ABSOLUTE = error "Operation PHA does not support ABSOLUTE addressing mode"
opPHA ABSOLUTE_X = error "Operation PHA does not support ABSOLUTE_X addressing mode"
opPHA ABSOLUTE_Y = error "Operation PHA does not support ABSOLUTE_Y addressing mode"
opPHA INDIRECT = error "Operation PHA does not support INDIRECT addressing mode"
opPHA INDIRECT_X = error "Operation PHA does not support INDIRECT_X addressing mode"
opPHA INDIRECT_Y = error "Operation PHA does not support INDIRECT_Y addressing mode"
opPHA IMPLICIT = do
    acc <- getACC 
    writeStack acc

opPHP ::ADDR_MODE -> StateT MOS6502 IO ()
opPHP ACCUMULATOR = error "Operation PHP does not support ACCUMULATOR addressing mode"
opPHP IMMEDIATE = error "Operation PHP does not support IMMEDIATE addressing mode"
opPHP ZEROPAGE = error "Operation PHP does not support ZEROPAGE addressing mode"
opPHP ZEROPAGE_X = error "Operation PHP does not support ZEROPAGE_X addressing mode"
opPHP ZEROPAGE_Y = error "Operation PHP does not support ZEROPAGE_Y addressing mode"
opPHP RELATIVE = error "Operation PHP does not support RELATIVE addressing mode"
opPHP ABSOLUTE = error "Operation PHP does not support ABSOLUTE addressing mode"
opPHP ABSOLUTE_X = error "Operation PHP does not support ABSOLUTE_X addressing mode"
opPHP ABSOLUTE_Y = error "Operation PHP does not support ABSOLUTE_Y addressing mode"
opPHP INDIRECT = error "Operation PHP does not support INDIRECT addressing mode"
opPHP INDIRECT_X = error "Operation PHP does not support INDIRECT_X addressing mode"
opPHP INDIRECT_Y = error "Operation PHP does not support INDIRECT_Y addressing mode"
opPHP IMPLICIT = do
    ps <- getPS 
    writeStack (setBit ps 4)

opPLA ::ADDR_MODE -> StateT MOS6502 IO ()
opPLA ACCUMULATOR = error "Operation PLA does not support ACCUMULATOR addressing mode"
opPLA IMMEDIATE = error "Operation PLA does not support IMMEDIATE addressing mode"
opPLA ZEROPAGE = error "Operation PLA does not support ZEROPAGE addressing mode"
opPLA ZEROPAGE_X = error "Operation PLA does not support ZEROPAGE_X addressing mode"
opPLA ZEROPAGE_Y = error "Operation PLA does not support ZEROPAGE_Y addressing mode"
opPLA RELATIVE = error "Operation PLA does not support RELATIVE addressing mode"
opPLA ABSOLUTE = error "Operation PLA does not support ABSOLUTE addressing mode"
opPLA ABSOLUTE_X = error "Operation PLA does not support ABSOLUTE_X addressing mode"
opPLA ABSOLUTE_Y = error "Operation PLA does not support ABSOLUTE_Y addressing mode"
opPLA INDIRECT = error "Operation PLA does not support INDIRECT addressing mode"
opPLA INDIRECT_X = error "Operation PLA does not support INDIRECT_X addressing mode"
opPLA INDIRECT_Y = error "Operation PLA does not support INDIRECT_Y addressing mode"
opPLA IMPLICIT = do
    acc <- readStack
    setACC acc
    setFlag ZERO (acc == 0)
    setFlag NEGATIVE (b7 acc)

opPLP ::ADDR_MODE -> StateT MOS6502 IO ()
opPLP ACCUMULATOR = error "Operation PLP does not support ACCUMULATOR addressing mode"
opPLP IMMEDIATE = error "Operation PLP does not support IMMEDIATE addressing mode"
opPLP ZEROPAGE = error "Operation PLP does not support ZEROPAGE addressing mode"
opPLP ZEROPAGE_X = error "Operation PLP does not support ZEROPAGE_X addressing mode"
opPLP ZEROPAGE_Y = error "Operation PLP does not support ZEROPAGE_Y addressing mode"
opPLP RELATIVE = error "Operation PLP does not support RELATIVE addressing mode"
opPLP ABSOLUTE = error "Operation PLP does not support ABSOLUTE addressing mode"
opPLP ABSOLUTE_X = error "Operation PLP does not support ABSOLUTE_X addressing mode"
opPLP ABSOLUTE_Y = error "Operation PLP does not support ABSOLUTE_Y addressing mode"
opPLP INDIRECT = error "Operation PLP does not support INDIRECT addressing mode"
opPLP INDIRECT_X = error "Operation PLP does not support INDIRECT_X addressing mode"
opPLP INDIRECT_Y = error "Operation PLP does not support INDIRECT_Y addressing mode"
opPLP IMPLICIT = do
    ps <- readStack
    let ps' = setBit ps 5
    let ps'' = clearBit ps' 4
    setPS ps''

opROL ::ADDR_MODE -> StateT MOS6502 IO ()
opROL IMPLICIT = error "Operation ROL does not support IMPLICIT addressing mode"
opROL IMMEDIATE = error "Operation ROL does not support IMMEDIATE addressing mode"
opROL ZEROPAGE_Y = error "Operation ROL does not support ZEROPAGE_Y addressing mode"
opROL RELATIVE = error "Operation ROL does not support RELATIVE addressing mode"
opROL ABSOLUTE_Y = error "Operation ROL does not support ABSOLUTE_Y addressing mode"
opROL INDIRECT = error "Operation ROL does not support INDIRECT addressing mode"
opROL INDIRECT_X = error "Operation ROL does not support INDIRECT_X addressing mode"
opROL INDIRECT_Y = error "Operation ROL does not support INDIRECT_Y addressing mode"
opROL ACCUMULATOR = do
    acc <- getACC 
    carry_flag <- getFlag CARRY
    let new_carry = b7 acc
    let bit0 = if carry_flag then (0x01 :: Word8) else (0x00 :: Word8)
    let acc' = (acc .<<. 1) .|. bit0
    setACC acc'
    setFlag ZERO (acc' == 0)
    setFlag NEGATIVE (b7 acc')
    setFlag CARRY new_carry
opROL addr_mode = do
    addr <- getAddr addr_mode
    byte <- readByte addr
    carry_flag <- getFlag CARRY
    let new_carry = b7 byte
    let bit0 = if carry_flag then (0x01 :: Word8) else (0x00 :: Word8)
    let byte' = (byte .<<. 1) .|. bit0
    writeByte addr byte'
    setFlag ZERO (byte' == 0)
    setFlag NEGATIVE (b7 byte')
    setFlag CARRY new_carry

opROR ::ADDR_MODE -> StateT MOS6502 IO ()
opROR IMPLICIT = error "Operation ROR does not support IMPLICIT addressing mode"
opROR IMMEDIATE = error "Operation ROR does not support IMMEDIATE addressing mode"
opROR ZEROPAGE_Y = error "Operation ROR does not support ZEROPAGE_Y addressing mode"
opROR RELATIVE = error "Operation ROR does not support RELATIVE addressing mode"
opROR ABSOLUTE_Y = error "Operation ROR does not support ABSOLUTE_Y addressing mode"
opROR INDIRECT = error "Operation ROR does not support INDIRECT addressing mode"
opROR INDIRECT_X = error "Operation ROR does not support INDIRECT_X addressing mode"
opROR INDIRECT_Y = error "Operation ROR does not support INDIRECT_Y addressing mode"
opROR ACCUMULATOR = do
    acc <- getACC 
    carry_flag <- getFlag CARRY
    let new_carry = b0 acc
    let bit0 = if carry_flag then (0x80 :: Word8) else (0x00 :: Word8)
    let acc' = (acc .>>. 1) .|. bit0
    setACC acc'
    setFlag ZERO (acc' == 0)
    setFlag NEGATIVE (b7 acc')
    setFlag CARRY new_carry
opROR addr_mode = do
    addr <- getAddr addr_mode
    byte <- readByte addr
    carry_flag <- getFlag CARRY
    let new_carry = b0 byte
    let bit0 = if carry_flag then (0x80 :: Word8) else (0x00 :: Word8)
    let byte' = (byte .>>. 1) .|. bit0
    writeByte addr byte'
    setFlag ZERO (byte' == 0)
    setFlag NEGATIVE (b7 byte')
    setFlag CARRY new_carry

opRTI ::ADDR_MODE -> StateT MOS6502 IO ()
opRTI ACCUMULATOR = error "Operation RTI does not support ACCUMULATOR addressing mode"
opRTI IMMEDIATE = error "Operation RTI does not support IMMEDIATE addressing mode"
opRTI ZEROPAGE = error "Operation RTI does not support ZEROPAGE addressing mode"
opRTI ZEROPAGE_X = error "Operation RTI does not support ZEROPAGE_X addressing mode"
opRTI ZEROPAGE_Y = error "Operation RTI does not support ZEROPAGE_Y addressing mode"
opRTI RELATIVE = error "Operation RTI does not support RELATIVE addressing mode"
opRTI ABSOLUTE = error "Operation RTI does not support ABSOLUTE addressing mode"
opRTI ABSOLUTE_X = error "Operation RTI does not support ABSOLUTE_X addressing mode"
opRTI ABSOLUTE_Y = error "Operation RTI does not support ABSOLUTE_Y addressing mode"
opRTI INDIRECT = error "Operation RTI does not support INDIRECT addressing mode"
opRTI INDIRECT_X = error "Operation RTI does not support INDIRECT_X addressing mode"
opRTI INDIRECT_Y = error "Operation RTI does not support INDIRECT_Y addressing mode"
opRTI IMPLICIT = do
    ps <- readStack
    let ps' = setBit ps 5
    let ps'' = clearBit ps' 4

    pclb <- readStack
    pchb <- readStack
    let pc = joinBytes pchb pclb
    setPS ps''
    setPC pc

opRTS ::ADDR_MODE -> StateT MOS6502 IO ()
opRTS ACCUMULATOR = error "Operation RTS does not support ACCUMULATOR addressing mode"
opRTS IMMEDIATE = error "Operation RTS does not support IMMEDIATE addressing mode"
opRTS ZEROPAGE = error "Operation RTS does not support ZEROPAGE addressing mode"
opRTS ZEROPAGE_X = error "Operation RTS does not support ZEROPAGE_X addressing mode"
opRTS ZEROPAGE_Y = error "Operation RTS does not support ZEROPAGE_Y addressing mode"
opRTS RELATIVE = error "Operation RTS does not support RELATIVE addressing mode"
opRTS ABSOLUTE = error "Operation RTS does not support ABSOLUTE addressing mode"
opRTS ABSOLUTE_X = error "Operation RTS does not support ABSOLUTE_X addressing mode"
opRTS ABSOLUTE_Y = error "Operation RTS does not support ABSOLUTE_Y addressing mode"
opRTS INDIRECT = error "Operation RTS does not support INDIRECT addressing mode"
opRTS INDIRECT_X = error "Operation RTS does not support INDIRECT_X addressing mode"
opRTS INDIRECT_Y = error "Operation RTS does not support INDIRECT_Y addressing mode"
opRTS IMPLICIT = do
    pclb <- readStack
    pchb <- readStack
    let pc = (joinBytes pchb pclb) + 1 -- Read JSR to understand this addition
    setPC pc

opSBC ::ADDR_MODE -> StateT MOS6502 IO ()
opSBC IMPLICIT = error "Operation SBC does not support IMPLICIT addressing mode"
opSBC ACCUMULATOR = error "Operation SBC does not support ACCUMULATOR addressing mode"
opSBC ZEROPAGE_Y = error "Operation SBC does not support ZEROPAGE_Y addressing mode"
opSBC RELATIVE = error "Operation SBC does not support RELATIVE addressing mode"
opSBC INDIRECT = error "Operation SBC does not support INDIRECT addressing mode"
opSBC addr_mode = do
    acc <- getACC 
    carry_flag <- getFlag $ CARRY -- Get the Accumulator registers prior to changes
    decimal_flag <- getFlag $ DECIMAL_MODE
    addr <- getAddr addr_mode -- Get the address given the addressing mode
    byte <- readByte addr -- Read byte from the Bus
    let carry = if carry_flag then 1 else 0 :: Word8
    let iacc = fromIntegral acc :: Int
    let ibyte = fromIntegral byte :: Int
    let icarry = fromIntegral carry :: Int
    let iresult = iacc - ibyte - (1 - icarry)
    setFlag ZERO (iresult == 0)
    if decimal_flag
        then do
            let not_carry = carry `xor` 0x1
            let acc16 = fromIntegral acc :: Word16
            let operand16 = fromIntegral byte 
            let nc16 = fromIntegral not_carry :: Word16
            let decimal_result = acc16 - operand16 - nc16

            let temp161 = (acc16 .&. 0xf) - (operand16 .&. 0xf) - nc16
            let temp162 = if temp161 > 0xf then temp161 - 0x6 else temp161
            let tempadd = if temp162 > 0x0f then 0xfff0 else 0x00
            let temp163 = (temp162 .&. 0x0f) + tempadd
            let temp164 = temp163 + (acc16 .&. 0xf0) - (operand16 .&. 0xf0)
            let temp165 = if temp164 > 0xff then temp164 - 0x60 else temp164
            let r = fromIntegral temp165 :: Word8
            let ovf = ((decimal_result `xor` acc16) .&. (complement (decimal_result `xor` operand16))) .&. 0x80
            let ovf2 = ovf .>>. 1
            setFlag OVERFLOW (ovf2 /= 0)
            setFlag CARRY (not (temp164 > 0xFF))
            setFlag NEGATIVE (b7 decimal_result)
            setFlag ZERO (decimal_result .&. 0xFF == 0)
            setACC r
        else do
            let operand = ibyte `xor` 0x00FF
            let temp = iacc + operand + icarry
            setFlag NEGATIVE (b7 temp)
            setFlag CARRY (temp .&. 0xFF00 /= 0)
            setFlag OVERFLOW (b7 ((temp `xor` iacc) .&. (temp `xor` operand)))
            let acc' = fromIntegral (temp .&. 0xFF) :: Word8
            setACC acc'
            return ()

opSEC ::ADDR_MODE -> StateT MOS6502 IO ()
opSEC ACCUMULATOR = error "Operation SEC does not support ACCUMULATOR addressing mode"
opSEC IMMEDIATE = error "Operation SEC does not support IMMEDIATE addressing mode"
opSEC ZEROPAGE = error "Operation SEC does not support ZEROPAGE addressing mode"
opSEC ZEROPAGE_X = error "Operation SEC does not support ZEROPAGE_X addressing mode"
opSEC ZEROPAGE_Y = error "Operation SEC does not support ZEROPAGE_Y addressing mode"
opSEC RELATIVE = error "Operation SEC does not support RELATIVE addressing mode"
opSEC ABSOLUTE = error "Operation SEC does not support ABSOLUTE addressing mode"
opSEC ABSOLUTE_X = error "Operation SEC does not support ABSOLUTE_X addressing mode"
opSEC ABSOLUTE_Y = error "Operation SEC does not support ABSOLUTE_Y addressing mode"
opSEC INDIRECT = error "Operation SEC does not support INDIRECT addressing mode"
opSEC INDIRECT_X = error "Operation SEC does not support INDIRECT_X addressing mode"
opSEC INDIRECT_Y = error "Operation SEC does not support INDIRECT_Y addressing mode"
opSEC IMPLICIT = do
    setFlag CARRY True

opSED ::ADDR_MODE -> StateT MOS6502 IO ()
opSED ACCUMULATOR = error "Operation SED does not support ACCUMULATOR addressing mode"
opSED IMMEDIATE = error "Operation SED does not support IMMEDIATE addressing mode"
opSED ZEROPAGE = error "Operation SED does not support ZEROPAGE addressing mode"
opSED ZEROPAGE_X = error "Operation SED does not support ZEROPAGE_X addressing mode"
opSED ZEROPAGE_Y = error "Operation SED does not support ZEROPAGE_Y addressing mode"
opSED RELATIVE = error "Operation SED does not support RELATIVE addressing mode"
opSED ABSOLUTE = error "Operation SED does not support ABSOLUTE addressing mode"
opSED ABSOLUTE_X = error "Operation SED does not support ABSOLUTE_X addressing mode"
opSED ABSOLUTE_Y = error "Operation SED does not support ABSOLUTE_Y addressing mode"
opSED INDIRECT = error "Operation SED does not support INDIRECT addressing mode"
opSED INDIRECT_X = error "Operation SED does not support INDIRECT_X addressing mode"
opSED INDIRECT_Y = error "Operation SED does not support INDIRECT_Y addressing mode"
opSED IMPLICIT = do
    setFlag DECIMAL_MODE True

opSEI ::ADDR_MODE -> StateT MOS6502 IO ()
opSEI ACCUMULATOR = error "Operation SEI does not support ACCUMULATOR addressing mode"
opSEI IMMEDIATE = error "Operation SEI does not support IMMEDIATE addressing mode"
opSEI ZEROPAGE = error "Operation SEI does not support ZEROPAGE addressing mode"
opSEI ZEROPAGE_X = error "Operation SEI does not support ZEROPAGE_X addressing mode"
opSEI ZEROPAGE_Y = error "Operation SEI does not support ZEROPAGE_Y addressing mode"
opSEI RELATIVE = error "Operation SEI does not support RELATIVE addressing mode"
opSEI ABSOLUTE = error "Operation SEI does not support ABSOLUTE addressing mode"
opSEI ABSOLUTE_X = error "Operation SEI does not support ABSOLUTE_X addressing mode"
opSEI ABSOLUTE_Y = error "Operation SEI does not support ABSOLUTE_Y addressing mode"
opSEI INDIRECT = error "Operation SEI does not support INDIRECT addressing mode"
opSEI INDIRECT_X = error "Operation SEI does not support INDIRECT_X addressing mode"
opSEI INDIRECT_Y = error "Operation SEI does not support INDIRECT_Y addressing mode"
opSEI IMPLICIT = do
    setFlag INTERRUPT_DISABLE True

opSTA ::ADDR_MODE -> StateT MOS6502 IO ()
opSTA IMPLICIT = error "Operation STA does not support IMPLICIT addressing mode"
opSTA ACCUMULATOR = error "Operation STA does not support ACCUMULATOR addressing mode"
opSTA IMMEDIATE = error "Operation STA does not support IMMEDIATE addressing mode"
opSTA ZEROPAGE_Y = error "Operation STA does not support ZEROPAGE_Y addressing mode"
opSTA RELATIVE = error "Operation STA does not support RELATIVE addressing mode"
opSTA INDIRECT = error "Operation STA does not support INDIRECT addressing mode"
opSTA addr_mode = do
    addr <- getAddr addr_mode
    acc <- getACC 
    writeByte addr acc

opSTX ::ADDR_MODE -> StateT MOS6502 IO ()
opSTX IMPLICIT = error "Operation STX does not support IMPLICIT addressing mode"
opSTX ACCUMULATOR = error "Operation STX does not support ACCUMULATOR addressing mode"
opSTX IMMEDIATE = error "Operation STX does not support IMMEDIATE addressing mode"
opSTX ZEROPAGE_X = error "Operation STX does not support ZEROPAGE_X addressing mode"
opSTX RELATIVE = error "Operation STX does not support RELATIVE addressing mode"
opSTX ABSOLUTE_X = error "Operation STX does not support ABSOLUTE_X addressing mode"
opSTX ABSOLUTE_Y = error "Operation STX does not support ABSOLUTE_Y addressing mode"
opSTX INDIRECT = error "Operation STX does not support INDIRECT addressing mode"
opSTX INDIRECT_X = error "Operation STX does not support INDIRECT_X addressing mode"
opSTX INDIRECT_Y = error "Operation STX does not support INDIRECT_Y addressing mode"
opSTX addr_mode = do
    addr <- getAddr addr_mode
    regx <- getIDX 
    writeByte addr regx

opSTY ::ADDR_MODE -> StateT MOS6502 IO ()
opSTY IMPLICIT = error "Operation STY does not support IMPLICIT addressing mode"
opSTY ACCUMULATOR = error "Operation STY does not support ACCUMULATOR addressing mode"
opSTY IMMEDIATE = error "Operation STY does not support IMMEDIATE addressing mode"
opSTY ZEROPAGE_Y = error "Operation STY does not support ZEROPAGE_X addressing mode"
opSTY RELATIVE = error "Operation STY does not support RELATIVE addressing mode"
opSTY ABSOLUTE_X = error "Operation STY does not support ABSOLUTE_X addressing mode"
opSTY ABSOLUTE_Y = error "Operation STY does not support ABSOLUTE_Y addressing mode"
opSTY INDIRECT = error "Operation STY does not support INDIRECT addressing mode"
opSTY INDIRECT_X = error "Operation STY does not support INDIRECT_X addressing mode"
opSTY INDIRECT_Y = error "Operation STY does not support INDIRECT_Y addressing mode"
opSTY addr_mode = do
    addr <- getAddr addr_mode
    regy <- getIDY 
    writeByte addr regy

opTAX ::ADDR_MODE -> StateT MOS6502 IO ()
opTAX ACCUMULATOR = error "Operation TAX does not support ACCUMULATOR addressing mode"
opTAX IMMEDIATE = error "Operation TAX does not support IMMEDIATE addressing mode"
opTAX ZEROPAGE = error "Operation TAX does not support ZEROPAGE addressing mode"
opTAX ZEROPAGE_X = error "Operation TAX does not support ZEROPAGE_X addressing mode"
opTAX ZEROPAGE_Y = error "Operation TAX does not support ZEROPAGE_Y addressing mode"
opTAX RELATIVE = error "Operation TAX does not support RELATIVE addressing mode"
opTAX ABSOLUTE = error "Operation TAX does not support ABSOLUTE addressing mode"
opTAX ABSOLUTE_X = error "Operation TAX does not support ABSOLUTE_X addressing mode"
opTAX ABSOLUTE_Y = error "Operation TAX does not support ABSOLUTE_Y addressing mode"
opTAX INDIRECT = error "Operation TAX does not support INDIRECT addressing mode"
opTAX INDIRECT_X = error "Operation TAX does not support INDIRECT_X addressing mode"
opTAX INDIRECT_Y = error "Operation TAX does not support INDIRECT_Y addressing mode"
opTAX IMPLICIT = do
    acc <- getACC 
    setIDX acc
    setFlag ZERO (acc == 0)
    setFlag NEGATIVE (b7 acc)

opTAY ::ADDR_MODE -> StateT MOS6502 IO ()
opTAY ACCUMULATOR = error "Operation TAY does not support ACCUMULATOR addressing mode"
opTAY IMMEDIATE = error "Operation TAY does not support IMMEDIATE addressing mode"
opTAY ZEROPAGE = error "Operation TAY does not support ZEROPAGE addressing mode"
opTAY ZEROPAGE_X = error "Operation TAY does not support ZEROPAGE_X addressing mode"
opTAY ZEROPAGE_Y = error "Operation TAY does not support ZEROPAGE_Y addressing mode"
opTAY RELATIVE = error "Operation TAY does not support RELATIVE addressing mode"
opTAY ABSOLUTE = error "Operation TAY does not support ABSOLUTE addressing mode"
opTAY ABSOLUTE_X = error "Operation TAY does not support ABSOLUTE_X addressing mode"
opTAY ABSOLUTE_Y = error "Operation TAY does not support ABSOLUTE_Y addressing mode"
opTAY INDIRECT = error "Operation TAY does not support INDIRECT addressing mode"
opTAY INDIRECT_X = error "Operation TAY does not support INDIRECT_X addressing mode"
opTAY INDIRECT_Y = error "Operation TAY does not support INDIRECT_Y addressing mode"
opTAY IMPLICIT = do
    acc <- getACC 
    setIDY acc
    setFlag ZERO (acc == 0)
    setFlag NEGATIVE (b7 acc)

opTSX ::ADDR_MODE -> StateT MOS6502 IO ()
opTSX ACCUMULATOR = error "Operation TSX does not support ACCUMULATOR addressing mode"
opTSX IMMEDIATE = error "Operation TSX does not support IMMEDIATE addressing mode"
opTSX ZEROPAGE = error "Operation TSX does not support ZEROPAGE addressing mode"
opTSX ZEROPAGE_X = error "Operation TSX does not support ZEROPAGE_X addressing mode"
opTSX ZEROPAGE_Y = error "Operation TSX does not support ZEROPAGE_Y addressing mode"
opTSX RELATIVE = error "Operation TSX does not support RELATIVE addressing mode"
opTSX ABSOLUTE = error "Operation TSX does not support ABSOLUTE addressing mode"
opTSX ABSOLUTE_X = error "Operation TSX does not support ABSOLUTE_X addressing mode"
opTSX ABSOLUTE_Y = error "Operation TSX does not support ABSOLUTE_Y addressing mode"
opTSX INDIRECT = error "Operation TSX does not support INDIRECT addressing mode"
opTSX INDIRECT_X = error "Operation TSX does not support INDIRECT_X addressing mode"
opTSX INDIRECT_Y = error "Operation TSX does not support INDIRECT_Y addressing mode"
opTSX IMPLICIT = do
    sp <- getSP 
    setIDX sp
    setFlag ZERO (sp == 0)
    setFlag NEGATIVE (b7 sp)

opTXA ::ADDR_MODE -> StateT MOS6502 IO ()
opTXA ACCUMULATOR = error "Operation TXA does not support ACCUMULATOR addressing mode"
opTXA IMMEDIATE = error "Operation TXA does not support IMMEDIATE addressing mode"
opTXA ZEROPAGE = error "Operation TXA does not support ZEROPAGE addressing mode"
opTXA ZEROPAGE_X = error "Operation TXA does not support ZEROPAGE_X addressing mode"
opTXA ZEROPAGE_Y = error "Operation TXA does not support ZEROPAGE_Y addressing mode"
opTXA RELATIVE = error "Operation TXA does not support RELATIVE addressing mode"
opTXA ABSOLUTE = error "Operation TXA does not support ABSOLUTE addressing mode"
opTXA ABSOLUTE_X = error "Operation TXA does not support ABSOLUTE_X addressing mode"
opTXA ABSOLUTE_Y = error "Operation TXA does not support ABSOLUTE_Y addressing mode"
opTXA INDIRECT = error "Operation TXA does not support INDIRECT addressing mode"
opTXA INDIRECT_X = error "Operation TXA does not support INDIRECT_X addressing mode"
opTXA INDIRECT_Y = error "Operation TXA does not support INDIRECT_Y addressing mode"
opTXA IMPLICIT = do
    xreg <- getIDX 
    setACC xreg
    setFlag ZERO (xreg == 0)
    setFlag NEGATIVE (b7 xreg)

opTXS ACCUMULATOR = error "Operation TXS does not support ACCUMULATOR addressing mode"
opTXS IMMEDIATE = error "Operation TXS does not support IMMEDIATE addressing mode"
opTXS ZEROPAGE = error "Operation TXS does not support ZEROPAGE addressing mode"
opTXS ZEROPAGE_X = error "Operation TXS does not support ZEROPAGE_X addressing mode"
opTXS ZEROPAGE_Y = error "Operation TXS does not support ZEROPAGE_Y addressing mode"
opTXS RELATIVE = error "Operation TXS does not support RELATIVE addressing mode"
opTXS ABSOLUTE = error "Operation TXS does not support ABSOLUTE addressing mode"
opTXS ABSOLUTE_X = error "Operation TXS does not support ABSOLUTE_X addressing mode"
opTXS ABSOLUTE_Y = error "Operation TXS does not support ABSOLUTE_Y addressing mode"
opTXS INDIRECT = error "Operation TXS does not support INDIRECT addressing mode"
opTXS INDIRECT_X = error "Operation TXS does not support INDIRECT_X addressing mode"
opTXS INDIRECT_Y = error "Operation TXS does not support INDIRECT_Y addressing mode"
opTXS IMPLICIT = do
    xreg <- getIDX 
    setSP xreg

opTYA ::ADDR_MODE -> StateT MOS6502 IO ()
opTYA ACCUMULATOR = error "Operation TYA does not support ACCUMULATOR addressing mode"
opTYA IMMEDIATE = error "Operation TYA does not support IMMEDIATE addressing mode"
opTYA ZEROPAGE = error "Operation TYA does not support ZEROPAGE addressing mode"
opTYA ZEROPAGE_X = error "Operation TYA does not support ZEROPAGE_X addressing mode"
opTYA ZEROPAGE_Y = error "Operation TYA does not support ZEROPAGE_Y addressing mode"
opTYA RELATIVE = error "Operation TYA does not support RELATIVE addressing mode"
opTYA ABSOLUTE = error "Operation TYA does not support ABSOLUTE addressing mode"
opTYA ABSOLUTE_X = error "Operation TYA does not support ABSOLUTE_X addressing mode"
opTYA ABSOLUTE_Y = error "Operation TYA does not support ABSOLUTE_Y addressing mode"
opTYA INDIRECT = error "Operation TYA does not support INDIRECT addressing mode"
opTYA INDIRECT_X = error "Operation TYA does not support INDIRECT_X addressing mode"
opTYA INDIRECT_Y = error "Operation TYA does not support INDIRECT_Y addressing mode"
opTYA IMPLICIT = do
    yreg <- getIDY 
    setACC yreg
    setFlag ZERO (yreg == 0)
    setFlag NEGATIVE (b7 yreg)


-- Info

opInfo :: Word8 -> Maybe (String, ADDR_MODE)
opInfo 0x69 = Just ("ADC", IMMEDIATE)
opInfo 0x65 = Just ("ADC", ZEROPAGE)
opInfo 0x75 = Just ("ADC", ZEROPAGE_X)
opInfo 0x6D = Just ("ADC", ABSOLUTE)
opInfo 0x7D = Just ("ADC", ABSOLUTE_X)
opInfo 0x79 = Just ("ADC", ABSOLUTE_Y)
opInfo 0x61 = Just ("ADC", INDIRECT_X)
opInfo 0x71 = Just ("ADC", INDIRECT_Y)
opInfo 0x29 = Just ("AND", IMMEDIATE)
opInfo 0x25 = Just ("AND", ZEROPAGE)
opInfo 0x35 = Just ("AND", ZEROPAGE_X)
opInfo 0x2D = Just ("AND", ABSOLUTE)
opInfo 0x3D = Just ("AND", ABSOLUTE_X)
opInfo 0x39 = Just ("AND", ABSOLUTE_Y)
opInfo 0x21 = Just ("AND", INDIRECT_X)
opInfo 0x31 = Just ("AND", INDIRECT_Y)
opInfo 0x0A = Just ("ASL", ACCUMULATOR)
opInfo 0x06 = Just ("ASL", ZEROPAGE)
opInfo 0x16 = Just ("ASL", ZEROPAGE_X)
opInfo 0x0E = Just ("ASL", ABSOLUTE)
opInfo 0x1E = Just ("ASL", ABSOLUTE_X)
opInfo 0x90 = Just ("BCC", RELATIVE)
opInfo 0xB0 = Just ("BCS", RELATIVE)
opInfo 0xF0 = Just ("BEQ", RELATIVE)
opInfo 0x24 = Just ("BIT", ZEROPAGE)
opInfo 0x2C = Just ("BIT", ABSOLUTE)
opInfo 0x30 = Just ("BMI", RELATIVE)
opInfo 0xD0 = Just ("BNE", RELATIVE)
opInfo 0x10 = Just ("BPL", RELATIVE)
opInfo 0x00 = Just ("BRK", IMPLICIT)
opInfo 0x50 = Just ("BVC", RELATIVE)
opInfo 0x70 = Just ("BVS", RELATIVE)
opInfo 0x18 = Just ("CLC", IMPLICIT)
opInfo 0xD8 = Just ("CLD", IMPLICIT)
opInfo 0x58 = Just ("CLI", IMPLICIT)
opInfo 0xB8 = Just ("CLV", IMPLICIT)
opInfo 0xC9 = Just ("CMP", IMMEDIATE)
opInfo 0xC5 = Just ("CMP", ZEROPAGE)
opInfo 0xD5 = Just ("CMP", ZEROPAGE_X)
opInfo 0xCD = Just ("CMP", ABSOLUTE)
opInfo 0xDD = Just ("CMP", ABSOLUTE_X)
opInfo 0xD9 = Just ("CMP", ABSOLUTE_Y)
opInfo 0xC1 = Just ("CMP", INDIRECT_X)
opInfo 0xD1 = Just ("CMP", INDIRECT_Y)
opInfo 0xE0 = Just ("CPX", IMMEDIATE)
opInfo 0xE4 = Just ("CPX", ZEROPAGE)
opInfo 0xEC = Just ("CPX", ABSOLUTE)
opInfo 0xC0 = Just ("CPY", IMMEDIATE)
opInfo 0xC4 = Just ("CPY", ZEROPAGE)
opInfo 0xCC = Just ("CPY", ABSOLUTE)
opInfo 0xC6 = Just ("DEC", ZEROPAGE)
opInfo 0xD6 = Just ("DEC", ZEROPAGE_X)
opInfo 0xCE = Just ("DEC", ABSOLUTE)
opInfo 0xDE = Just ("DEC", ABSOLUTE_X)
opInfo 0xCA = Just ("DEX", IMPLICIT)
opInfo 0x88 = Just ("DEY", IMPLICIT)
opInfo 0x49 = Just ("EOR", IMMEDIATE)
opInfo 0x45 = Just ("EOR", ZEROPAGE)
opInfo 0x55 = Just ("EOR", ZEROPAGE_X)
opInfo 0x4D = Just ("EOR", ABSOLUTE)
opInfo 0x5D = Just ("EOR", ABSOLUTE_X)
opInfo 0x59 = Just ("EOR", ABSOLUTE_Y)
opInfo 0x41 = Just ("EOR", INDIRECT_X)
opInfo 0x51 = Just ("EOR", INDIRECT_Y)
opInfo 0xE6 = Just ("INC", ZEROPAGE)
opInfo 0xF6 = Just ("INC", ZEROPAGE_X)
opInfo 0xEE = Just ("INC", ABSOLUTE)
opInfo 0xFE = Just ("INC", ABSOLUTE_X)
opInfo 0xE8 = Just ("INX", IMPLICIT)
opInfo 0xC8 = Just ("INY", IMPLICIT)
opInfo 0x4C = Just ("JMP", ABSOLUTE)
opInfo 0x6C = Just ("JMP", INDIRECT)
opInfo 0x20 = Just ("JSR", ABSOLUTE)
opInfo 0xA9 = Just ("LDA", IMMEDIATE)
opInfo 0xA5 = Just ("LDA", ZEROPAGE)
opInfo 0xB5 = Just ("LDA", ZEROPAGE_X)
opInfo 0xAD = Just ("LDA", ABSOLUTE)
opInfo 0xBD = Just ("LDA", ABSOLUTE_X)
opInfo 0xB9 = Just ("LDA", ABSOLUTE_Y)
opInfo 0xA1 = Just ("LDA", INDIRECT_X)
opInfo 0xB1 = Just ("LDA", INDIRECT_Y)
opInfo 0xA2 = Just ("LDX", IMMEDIATE)
opInfo 0xA6 = Just ("LDX", ZEROPAGE)
opInfo 0xB6 = Just ("LDX", ZEROPAGE_Y)
opInfo 0xAE = Just ("LDX", ABSOLUTE)
opInfo 0xBE = Just ("LDX", ABSOLUTE_Y)
opInfo 0xA0 = Just ("LDY", IMMEDIATE)
opInfo 0xA4 = Just ("LDY", ZEROPAGE)
opInfo 0xB4 = Just ("LDY", ZEROPAGE_X)
opInfo 0xAC = Just ("LDY", ABSOLUTE)
opInfo 0xBC = Just ("LDY", ABSOLUTE_X)
opInfo 0x4A = Just ("LSR", ACCUMULATOR)
opInfo 0x46 = Just ("LSR", ZEROPAGE)
opInfo 0x56 = Just ("LSR", ZEROPAGE_X)
opInfo 0x4E = Just ("LSR", ABSOLUTE)
opInfo 0x5E = Just ("LSR", ABSOLUTE_X)
opInfo 0xEA = Just ("NOP", IMPLICIT)
opInfo 0x09 = Just ("ORA", IMMEDIATE)
opInfo 0x05 = Just ("ORA", ZEROPAGE)
opInfo 0x15 = Just ("ORA", ZEROPAGE_X)
opInfo 0x0D = Just ("ORA", ABSOLUTE)
opInfo 0x1D = Just ("ORA", ABSOLUTE_X)
opInfo 0x19 = Just ("ORA", ABSOLUTE_Y)
opInfo 0x01 = Just ("ORA", INDIRECT_X)
opInfo 0x11 = Just ("ORA", INDIRECT_Y)
opInfo 0x48 = Just ("PHA", IMPLICIT)
opInfo 0x08 = Just ("PHP", IMPLICIT)
opInfo 0x68 = Just ("PLA", IMPLICIT)
opInfo 0x28 = Just ("PLP", IMPLICIT)
opInfo 0x2A = Just ("ROL", ACCUMULATOR)
opInfo 0x26 = Just ("ROL", ZEROPAGE)
opInfo 0x36 = Just ("ROL", ZEROPAGE_X)
opInfo 0x2E = Just ("ROL", ABSOLUTE)
opInfo 0x3E = Just ("ROL", ABSOLUTE_X)
opInfo 0x6A = Just ("ROR", ACCUMULATOR)
opInfo 0x66 = Just ("ROR", ZEROPAGE)
opInfo 0x76 = Just ("ROR", ZEROPAGE_X)
opInfo 0x6E = Just ("ROR", ABSOLUTE)
opInfo 0x7E = Just ("ROR", ABSOLUTE_X)
opInfo 0x40 = Just ("RTI", IMPLICIT)
opInfo 0x60 = Just ("RTS", IMPLICIT)
opInfo 0xE9 = Just ("SBC", IMMEDIATE)
opInfo 0xE5 = Just ("SBC", ZEROPAGE)
opInfo 0xF5 = Just ("SBC", ZEROPAGE_X)
opInfo 0xED = Just ("SBC", ABSOLUTE)
opInfo 0xFD = Just ("SBC", ABSOLUTE_X)
opInfo 0xF9 = Just ("SBC", ABSOLUTE_Y)
opInfo 0xE1 = Just ("SBC", INDIRECT_X)
opInfo 0xF1 = Just ("SBC", INDIRECT_Y)
opInfo 0x38 = Just ("SEC", IMPLICIT)
opInfo 0xF8 = Just ("SED", IMPLICIT)
opInfo 0x78 = Just ("SEI", IMPLICIT)
opInfo 0x85 = Just ("STA", ZEROPAGE)
opInfo 0x95 = Just ("STA", ZEROPAGE_X)
opInfo 0x8D = Just ("STA", ABSOLUTE)
opInfo 0x9D = Just ("STA", ABSOLUTE_X)
opInfo 0x99 = Just ("STA", ABSOLUTE_Y)
opInfo 0x81 = Just ("STA", INDIRECT_X)
opInfo 0x91 = Just ("STA", INDIRECT_Y)
opInfo 0x86 = Just ("STX", ZEROPAGE)
opInfo 0x96 = Just ("STX", ZEROPAGE_Y)
opInfo 0x8E = Just ("STX", ABSOLUTE)
opInfo 0x84 = Just ("STY", ZEROPAGE)
opInfo 0x94 = Just ("STY", ZEROPAGE_X)
opInfo 0x8C = Just ("STY", ABSOLUTE)
opInfo 0xAA = Just ("TAX", IMPLICIT)
opInfo 0xA8 = Just ("TAY", IMPLICIT)
opInfo 0xBA = Just ("TSX", IMPLICIT)
opInfo 0x8A = Just ("TXA", IMPLICIT)
opInfo 0x9A = Just ("TXS", IMPLICIT)
opInfo 0x98 = Just ("TYA", IMPLICIT)
opInfo opcode = Nothing


disassembleArg :: Interface -> ADDR_MODE -> Word16 -> IO (String, Word16)
disassembleArg _ IMPLICIT _ = return $ ("", 0)
disassembleArg _ ACCUMULATOR _ = return $ ("A", 0)
disassembleArg interface IMMEDIATE addr = do
    argval <- (iPeekByte interface) addr
    return ("#$" ++ toHex2 argval, 1)
disassembleArg interface ZEROPAGE addr = do
    argval <- (iPeekByte interface) addr
    return ("$" ++ toHex2 argval, 1)
disassembleArg interface ZEROPAGE_X addr = do
    argval <- (iPeekByte interface) addr
    return ("$" ++ toHex2 argval ++ ",X", 1)
disassembleArg interface ZEROPAGE_Y addr = do
    argval <- (iPeekByte interface) addr
    return ("$" ++ toHex2 argval ++ ",Y", 1)
disassembleArg interface RELATIVE addr = do
    argval <- (iPeekByte interface) addr
    let offset = (fromIntegral argval :: Int8)
    return ("*" ++ toHex2 argval ++ " [" ++ (show offset) ++ "]", 1)
disassembleArg interface ABSOLUTE addr = do
    a1 <- (iPeekByte interface) addr
    a2 <- (iPeekByte interface) (addr + 1)
    return ("$" ++ toHex2 a2 ++ toHex2 a1 ++ "", 2)
disassembleArg interface ABSOLUTE_X addr = do
    a1 <- (iPeekByte interface) addr
    a2 <- (iPeekByte interface) (addr + 1)
    return ("$" ++ toHex2 a2 ++ toHex2 a1 ++ ",X", 2)
disassembleArg interface ABSOLUTE_Y addr = do
    a1 <- (iPeekByte interface) addr
    a2 <- (iPeekByte interface) (addr + 1)
    return ("$" ++ toHex2 a2 ++ toHex2 a1 ++ ",Y", 2)
disassembleArg interface INDIRECT addr = do
    a1 <- (iPeekByte interface) addr
    a2 <- (iPeekByte interface) (addr + 1)
    return ("($" ++ toHex2 a2 ++ toHex2 a1 ++ ")", 2)
disassembleArg interface INDIRECT_X addr = do
    a <- (iPeekByte interface) addr
    return ("($" ++ toHex2 a ++ ", X)", 1)
disassembleArg interface INDIRECT_Y addr = do
    a <- (iPeekByte interface) addr
    return ("($" ++ toHex2 a ++ "), Y", 1)

disassemble :: Interface -> Word16 -> IO (String, Word16)
disassemble interface addr = do
    opcode <- (iPeekByte interface) addr
    let info = opInfo opcode
    case info of
        Just (opname, addr_mode) -> do
            (args, offset) <- disassembleArg interface addr_mode (addr + 1)
            return (opname ++ " " ++ args ++ (replicate (15 - length args) ' ') ++ show addr_mode, offset + 1)
        Nothing -> return $ ("", 1)

overflows :: Word16 -> Word16 -> Bool
overflows a b = s < a || s < b where
    s = a + b

disassembleL' :: Interface -> Word16 -> Word16 -> IO [(Word16, String)]
disassembleL' interface start end 
    | start >= end = return []
    | otherwise = do
        (str, offset) <- disassemble interface start
        let start' = if (overflows start offset) then end else start + offset
        rest <- disassembleL' interface start' end 
        return $ [(start, str)] ++ rest

disassembleM' :: Interface -> Word16 -> Word16 -> IO (Map.Map Word16 String)
disassembleM' interface start end = (Map.fromList) <$> (disassembleL' interface start end)


disassembleL :: MOS6502 -> Word16 -> Word16 -> IO [(Word16, String)]
disassembleL mos start end = disassembleL' (interface mos) start end


disassembleM :: MOS6502 -> Word16 -> Word16 -> IO (Map.Map Word16 String)
disassembleM mos start end = disassembleM' (interface mos) start end
