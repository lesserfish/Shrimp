{-# LANGUAGE ScopedTypeVariables #-}

module Shrimp.Bus where
import Data.Word
import Data.Int
import Control.Monad.State
import Data.Bits

class AbstractBus a where
    writeByte :: Word16 -> Word8 -> a -> a
    readByte :: Word16 -> a -> (a, Word8)

data REGISTER =     PC
                |   SP
                |   ACC
                |   IDX
                |   IDY
                |   PS deriving Show

data Registers = Registers  {   pc :: Word16
                            ,   sp :: Word8
                            ,   acc :: Word8
                            ,   idx :: Word8
                            ,   idy :: Word8
                            ,   ps :: Word8 
                            } deriving Show

data MOS6502 = MOS6502 {    mosRegisters :: Registers
                       ,    clock :: Int
                       ,    cycles :: Int
                       } deriving Show
data FLAG =     CARRY
            |   ZERO
            |   INTERRUPT_DISABLE
            |   DECIMAL_MODE
            |   BREAK_CMD
            |   OVERFLOW
            |   NEGATIVE deriving Show


data RegisterValue a = RegisterValue (Registers -> a)

class RegisterType a where
  readRegister :: REGISTER -> RegisterValue a
  writeRegister :: REGISTER -> a -> Registers -> Registers

instance RegisterType Word16 where
  readRegister PC = RegisterValue pc
  readRegister _ = error "Attempted to read Word8 as Word16"
  writeRegister PC val regs = regs { pc = val }
  writeRegister _ _ _ = error "Attempted to Write Word8 to Word16"
  
instance RegisterType Word8 where
  readRegister SP = RegisterValue sp
  readRegister ACC = RegisterValue acc
  readRegister IDX = RegisterValue idx
  readRegister IDY = RegisterValue idy
  readRegister PS = RegisterValue ps
  readRegister _ = error "Attempted to read Word16 as Word8"
  
  writeRegister SP val regs = regs { sp = val }
  writeRegister ACC val regs = regs { acc = val }
  writeRegister IDX val regs = regs { idx = val }
  writeRegister IDY val regs = regs { idy = val }
  writeRegister PS val regs = regs { ps = val }
  writeRegister _ _ _ = error "Attempted to write Word16 to Word8"

mapReg :: (AbstractBus a, RegisterType b) => REGISTER -> (b -> b) -> State (MOS6502, a) ()
mapReg reg func = do
  (cpu, bus) <- get
  let registers = mosRegisters cpu
      RegisterValue getter = readRegister reg
      currentVal = getter registers
      updatedVal = func currentVal
      registers' = writeRegister reg updatedVal registers
      cpu' = cpu {mosRegisters = registers'}
  put (cpu', bus)

setReg :: (AbstractBus a, RegisterType b) => REGISTER -> b -> State (MOS6502, a) ()
setReg reg value = mapReg reg (\_ -> value)

getReg :: (AbstractBus a, RegisterType b) => REGISTER -> State (MOS6502, a) b
getReg reg = do
    (mos6502, _) <- get
    let registers = mosRegisters mos6502
    let RegisterValue getter = readRegister reg
    let regval = getter registers
    return regval

getFlag :: FLAG -> MOS6502 -> Bool
getFlag flag = undefined

setFlag :: AbstractBus a => FLAG -> Bool -> State (MOS6502, a) ()
setFlag flag value = undefined

setFlagIf :: AbstractBus a => Bool -> FLAG -> Bool -> State (MOS6502, a) ()
setFlagIf condition flag value = do
    if condition then (setFlag flag value) else return ()

data Bus = Bus  {    busCPU :: MOS6502
                ,    busRAM :: [Word8]
                } deriving Show

pushCPU :: (MOS6502, Bus) -> Bus
pushCPU (cpu, bus) = bus {busCPU = cpu}

instance AbstractBus Bus where
    writeByte addr byte bus = undefined
    readByte addr bus = undefined

data ADDR_MODE =    IMPLICIT
                |   ACCUMULATOR
                |   IMMEDIATE
                |   ZEROPAGE
                |   ZEROPAGE_X
                |   ZEROPAGE_Y
                |   RELATIVE
                |   ABSOLUTE
                |   ABSOLUTE_X
                |   ABSOLUTE_Y
                |   INDIRECT
                |   INDEXED_INDIRECT
                |   INDIRECT_INDEXED deriving Show

joinBytes :: Word8 -> Word8 -> Word16
joinBytes hb lb = fromIntegral hb `shiftL` 8 .|. fromIntegral lb

getAddr :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) Word16

getAddr IMPLICIT = return 0
getAddr ACCUMULATOR = return 0

getAddr IMMEDIATE = do
    addr <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16
    setReg PC (addr + 1)
    return addr

getAddr ZEROPAGE = do
    cPC <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16
    lb <- mReadByte cPC
    let addr = joinBytes 0x00 lb
    setReg PC (cPC + 1)
    return addr

getAddr ZEROPAGE_X = do
    cPC <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16
    cIdx <- getReg IDX :: AbstractBus a1 => State (MOS6502, a1) Word8
    lb <- mReadByte cPC
    let addr = (joinBytes 0x00 lb) + (joinBytes 0x00 cIdx)
    setReg PC (cPC + 1)
    return addr

getAddr ZEROPAGE_Y = do
    cPC <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16
    cIdy <- getReg IDY :: AbstractBus a1 => State (MOS6502, a1) Word8
    lb <- mReadByte cPC
    let addr = (joinBytes 0x00 lb) + (joinBytes 0x00 cIdy)
    setReg PC (cPC + 1)
    return addr

getAddr RELATIVE = do
    cPC <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16
    offset <- mReadByte cPC
    let addr = if (testBit offset 7)  -- Test if the offset represents a positive or a negative number
                then cPC + 1 - (joinBytes 0x00 offset) -- If it represents a negative number, subtract it from cPC + 1
                else cPC + 1 + (joinBytes 0x00 offset) -- If it represents a positive number, add it to cPC + 1
    setReg PC (cPC + 1)
    return addr

getAddr ABSOLUTE = do
    cPC <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16
    lb <- mReadByte cPC
    hb <- mReadByte (cPC + 1)
    let addr = joinBytes hb lb
    setReg PC (cPC + 2)
    return addr

getAddr ABSOLUTE_X = do
    cPC <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16
    cIdx <- getReg IDX :: AbstractBus a1 => State (MOS6502, a1) Word8
    lb <- mReadByte cPC
    hb <- mReadByte (cPC + 1)
    let addr = (joinBytes hb lb) + (joinBytes 0x00 cIdx)
    setReg PC (cPC + 2)
    return addr

getAddr ABSOLUTE_Y = do
    cPC <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16
    cIdy <- getReg IDY :: AbstractBus a1 => State (MOS6502, a1) Word8
    lb <- mReadByte cPC
    hb <- mReadByte (cPC + 1)
    let addr = (joinBytes hb lb) + (joinBytes 0x00 cIdy)
    setReg PC (cPC + 2)
    return addr

getAddr INDIRECT = do
    cPC <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16
    addr_lb <- mReadByte cPC
    addr_hb <- mReadByte (cPC + 1)
    let addr1 = joinBytes addr_hb addr_lb
    lb <- mReadByte addr1
    hb <- mReadByte (addr1 + 1) 
    let addr = joinBytes hb lb
    setReg PC (cPC + 2)
    return addr

getAddr INDEXED_INDIRECT = do
    cPC <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16
    table_start <- mReadByte cPC                                               -- PC Holds the start address of a memory table
    table_offset <- getReg IDX :: AbstractBus a1 => State (MOS6502, a1) Word8  -- IDX Holds the offset from the start of the memory table
    let table_addr = table_start + table_offset                                -- Get table_addr = table_start + offset (Word8)
    lb <- mReadByte (joinBytes 0x00 table_addr)                                -- Read low byte
    hb <- mReadByte (joinBytes 0x00 (table_addr + 1))                          -- Read high byte
    let addr = joinBytes hb lb
    setReg PC (cPC + 1)
    return addr

getAddr INDIRECT_INDEXED = do
    cPC <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16
    table_lb <- mReadByte cPC                                                   -- PC contains the zero-page memory address which contains the low byte of the actual output     
    cIdy <- getReg IDY :: AbstractBus a1 => State (MOS6502, a1) Word8           -- IDY holds the offset from that address
    lb <- mReadByte (joinBytes 0x00 table_lb)                                   -- We read from the memory address held at PC
    let addr = (joinBytes 0x00 lb) + (joinBytes 0x00 cIdy)                      -- We add IDY to it
    setReg PC (cPC + 1)
    return addr
   
mReadByte :: AbstractBus a => Word16 -> State (MOS6502, a) Word8
mReadByte addr = do
    (mos6502, bus) <- get
    let (bus', byte) = readByte addr bus
    put (mos6502, bus')
    return byte
mWriteByte :: AbstractBus a => Word16 -> Word8 -> State (MOS6502, a) ()
mWriteByte addr byte = do
    (mos6502, bus) <- get
    let bus' = writeByte addr byte bus
    put (mos6502, bus')

opADC :: AbstractBus a => ADDR_MODE -> (MOS6502, a) -> (MOS6502, a)
opADC addr_mode (mos6502, bus) = (mos6502', bus') where
    (mos6502', bus') = execState ( do
        old_acc <- getReg ACC :: AbstractBus a1 => State (MOS6502, a1) Word8    -- Get the Accumulator registers prior to changes
        addr <- getAddr addr_mode                                               -- Get the address given the addressing mode
        byte <- mReadByte addr                                                  -- Read byte from the Bus
        mapReg ACC (+ byte)                                                     -- Add corresponding byte to Accumulator
        acc <- getReg ACC :: AbstractBus a1 => State (MOS6502, a1) Word8        -- Get the updated Accumulator
        setFlag ZERO (acc == 0)                                                 -- Sets the Zero flag if the result is equal to 0
        setFlag NEGATIVE (testBit acc 7)                                        -- Sets the Negative flag is the result is negative
        setFlag OVERFLOW (xor (testBit acc 7) (testBit old_acc 7))              -- Sets the Overflow negative if the seventh bit changes
        ) (mos6502, bus)

opINX :: AbstractBus a => ADDR_MODE -> (MOS6502, a) -> (MOS6502, a)
opINX IMPLICIT (mos6502, bus) = (mos6502', bus') where
    (mos6502', bus') = execState ( do
        mapReg IDX (+ (1 :: Word8))                                             -- Increases the X Register by one
        idx <- getReg IDX :: AbstractBus a1 => State (MOS6502, a1) Word8        -- Gets the updated X Register
        setFlag ZERO (idx == 0)                                                 -- Sets the Zero flag is the result is equal to 0
        setFlag NEGATIVE (testBit idx 7)                                        -- Sets the Negative flag is the result is negative
        ) (mos6502, bus)
opINX addr_mode _ = error ("Incompatible addressing mode: INX and " ++ show addr_mode)

