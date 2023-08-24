module Shrimp.Bus where
import Data.Word
import Data.Int
import Control.Monad.State
import Data.Bits
import GHC.IO.Exception (stackOverflow)

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

setRegIf :: (AbstractBus a, RegisterType b) => Bool -> REGISTER -> b -> State (MOS6502, a) ()
setRegIf condition reg value = if condition then setReg reg value else return ()


getReg :: (AbstractBus a, RegisterType b) => REGISTER -> State (MOS6502, a) b
getReg reg = do
    (mos6502, _) <- get
    let registers = mosRegisters mos6502
    let RegisterValue getter = readRegister reg
    let regval = getter registers
    return regval

getFlag :: AbstractBus a => FLAG -> State (MOS6502, a) Bool
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

splitBytes :: Word16 -> (Word8, Word8)
splitBytes byte = (hb, lb) where
   lb = fromIntegral (0x00FF .&. byte) 
   hb = fromIntegral ((shiftR byte 8) .&. 0x00FF)

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

getAddr :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) Word16

getAddr IMPLICIT = return 0                                                     -- Implicit does not require getAddr
getAddr ACCUMULATOR = return 0                                                  -- Accumulator does not require getAddr

getAddr IMMEDIATE = do
    addr <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16           -- The current position of the PC is precisely the address we are interested in
    setReg PC (addr + 1)
    return addr

getAddr ZEROPAGE = do
    cPC <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16            -- In Zero Page mode, PC contains the low byte of the address we are interested in
    lb <- mReadByte cPC                                                         -- Get the low byte
    let addr = joinBytes 0x00 lb
    setReg PC (cPC + 1)
    return addr

getAddr ZEROPAGE_X = do
    cPC <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16            
    xreg <- getReg IDX :: AbstractBus a1 => State (MOS6502, a1) Word8
    lb <- mReadByte cPC                                                         -- Get the low byte of the address
    let addr = (joinBytes 0x00 lb) + (joinBytes 0x00 xreg)                      -- Add IDX to the address
    setReg PC (cPC + 1)
    return addr

getAddr ZEROPAGE_Y = do
    cPC <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16
    yreg <- getReg IDY :: AbstractBus a1 => State (MOS6502, a1) Word8
    lb <- mReadByte cPC                                                         -- Get the low byte of the address
    let addr = (joinBytes 0x00 lb) + (joinBytes 0x00 yreg)                      -- Add IDY to the address
    setReg PC (cPC + 1)
    return addr

getAddr RELATIVE = do
    cPC <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16            -- Get the current position of PC
    offset <- mReadByte cPC                                                     -- Get the offset byte
    let addr = if (b7 offset)                                            -- Test if the offset represents a positive or a negative number
                then cPC + 1 - (joinBytes 0x00 offset)                          -- If it represents a negative number, subtract it from cPC + 1
                else cPC + 1 + (joinBytes 0x00 offset)                          -- If it represents a positive number, add it to cPC + 1
    setReg PC (cPC + 1)
    return addr

getAddr ABSOLUTE = do
    cPC <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16            -- PC holds the start address of where the actual address resides in memory
    lb <- mReadByte cPC                                                         -- Load the low byte of the address location
    hb <- mReadByte (cPC + 1)                                                   -- Load the high byte of the address location
    let addr = joinBytes hb lb
    setReg PC (cPC + 2)
    return addr

getAddr ABSOLUTE_X = do
    cPC <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16            -- PC holds the start address ofwhere the actual address resides in memory
    xreg <- getReg IDX :: AbstractBus a1 => State (MOS6502, a1) Word8           -- IDX is then added to the result
    lb <- mReadByte cPC                                                         -- Load the low byte of the address location
    hb <- mReadByte (cPC + 1)                                                   -- Load the high byte of the address location
    let addr = (joinBytes hb lb) + (joinBytes 0x00 xreg)                        -- Add the resulting 2-byte with the IDX register
    setReg PC (cPC + 2)
    return addr

getAddr ABSOLUTE_Y = do
    cPC <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16            -- PC holds the start address ofwhere the actual address resides in memory
    yreg <- getReg IDY :: AbstractBus a1 => State (MOS6502, a1) Word8           -- IDX is then added to the result
    lb <- mReadByte cPC                                                         -- Load the low byte of the address location
    hb <- mReadByte (cPC + 1)                                                   -- Load the high byte of the address location
    let addr = (joinBytes hb lb) + (joinBytes 0x00 yreg)                        -- Add the resulting 2-byte with the IDY register
    setReg PC (cPC + 2)
    return addr

getAddr INDIRECT = do
    cPC <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16            -- PC Holds the start address of of where the actual address resides in memory
    addr_lb <- mReadByte cPC                                                    -- Load the low byte of the address location
    addr_hb <- mReadByte (cPC + 1)                                              -- Load the high byte of the address location
    let addr1 = joinBytes addr_hb addr_lb
    lb <- mReadByte addr1                                                       -- Get the actual address from that location
    hb <- mReadByte (addr1 + 1) 
    let addr = joinBytes hb lb
    setReg PC (cPC + 2)
    return addr

getAddr INDEXED_INDIRECT = do
    cPC <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16
    table_start <- mReadByte cPC                                                -- PC Holds the start address of a memory table
    table_offset <- getReg IDX :: AbstractBus a1 => State (MOS6502, a1) Word8   -- IDX Holds the offset from the start of the memory table
    let table_addr = table_start + table_offset                                 -- Get table_addr = table_start + offset (Word8)
    lb <- mReadByte (joinBytes 0x00 table_addr)                                 -- Read low byte
    hb <- mReadByte (joinBytes 0x00 (table_addr + 1))                           -- Read high byte
    let addr = joinBytes hb lb
    setReg PC (cPC + 1)
    return addr

getAddr INDIRECT_INDEXED = do
    cPC <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16
    table_lb <- mReadByte cPC                                                   -- PC contains the zero-page memory address which contains the low byte of the actual output     
    yreg <- getReg IDY :: AbstractBus a1 => State (MOS6502, a1) Word8           -- IDY holds the offset from that address
    lb <- mReadByte (joinBytes 0x00 table_lb)                                   -- We read from the memory address held at PC
    let addr = (joinBytes 0x00 lb) + (joinBytes 0x00 yreg)                      -- We add IDY to it
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

mWriteStack :: AbstractBus a => Word8 -> State (MOS6502, a) ()
mWriteStack byte = do
    (mos6502, bus) <- get                                                       -- Get the current of the CPU
    sp <- getReg SP :: AbstractBus a1 => State (MOS6502, a1) Word8              -- Get the Stack Pointer
    let addr = 0x0100 + (joinBytes 0x00 sp)                                     -- Stack is between 0x0100 and 0x01FF
    mWriteByte addr byte                                                        -- Write byte to stack
    mapReg SP (\x -> (x :: Word8) - 1)                                          -- Decrement stack pointer

mReadStack :: AbstractBus a => State (MOS6502, a) Word8
mReadStack = do
    (mos6502, bus) <- get                                                       -- Get the current of the CPU
    sp <- getReg SP :: AbstractBus a1 => State (MOS6502, a1) Word8              -- Get the Stack Pointer
    let addr = 0x0100 + (joinBytes 0x00 sp)                                     -- Stack is between 0x0100 and 0x01FF
    mapReg SP (+ (1 :: Word8))                                                  -- Increment stack pointer
    mReadByte addr                                                              -- Read byte
    
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
-- opXXX INDEXED_INDIRECT = error "Operation XXX does not support INDEXED_INDIRECT addressing mode"
-- opXXX INDIRECT_INDEXED = error "Operation XXX does not support INDIRECT_INDEXED addressing mode"
--
--
-- Note: Instructions do not have to update the PC unless they use additional operands through the means of addressing modes.
-- The PC should be updated before the call to the instruction.

opADC :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
opADC IMPLICIT = error "Operation ADC does not support IMPLICIT addressing mode"
opADC ACCUMULATOR = error "Operation ADC does not support ACCUMULATOR addressing mode"
opADC ZEROPAGE_Y = error "Operation ADC does not support ZEROPAGE_Y addressing mode"
opADC RELATIVE = error "Operation ADC does not support RELATIVE addressing mode"
opADC INDIRECT = error "Operation ADC does not support INDIRECT addressing mode"
opADC addr_mode = do
        old_acc <- getReg ACC :: AbstractBus a1 => State (MOS6502, a1) Word8                        -- Get the Accumulator registers prior to changes
        carry_flag <- getFlag $ CARRY                                                               -- Get the Accumulator registers prior to changes
        let carry = if carry_flag then 1 else 0 :: Word8
        addr <- getAddr addr_mode                                                                   -- Get the address given the addressing mode
        byte <- mReadByte addr                                                                      -- Read byte from the Bus
        let operand = byte + carry
        mapReg ACC (+ operand)                                                                      -- Add corresponding byte to Accumulator
        acc <- getReg ACC :: AbstractBus a1 => State (MOS6502, a1) Word8                            -- Get the updated Accumulator
        setFlag ZERO (acc == 0)                                                                     -- Sets the Zero flag if the result is equal to 0
        setFlag NEGATIVE (b7 acc)                                                                   -- Sets the Negative flag is the result is negative
        setFlag OVERFLOW ((b7 old_acc) `xor` (b7 acc) && (b7 operand) `xor` (b7 acc))               -- Sets the Overflow flag

opAND :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
opAND IMPLICIT = error "Operation AND does not support IMPLICIT addressing mode"
opAND ACCUMULATOR = error "Operation AND does not support ACCUMULATOR addressing mode"
opAND ZEROPAGE_Y = error "Operation AND does not support ZEROPAGE_Y addressing mode"
opAND RELATIVE = error "Operation AND does not support RELATIVE addressing mode"
opAND INDIRECT = error "Operation AND does not support INDIRECT addressing mode"
opAND addr_mode = do
        old_acc <- getReg ACC :: AbstractBus a1 => State (MOS6502, a1) Word8    -- Get the Accumulator registers prior to changes
        addr <- getAddr addr_mode                                               -- Get the address given the addressing mode
        byte <- mReadByte addr                                                  -- Read byte from the Bus
        mapReg ACC (.&. byte)                                                   -- AND the corresponding byte to Accumulator
        acc <- getReg ACC :: AbstractBus a1 => State (MOS6502, a1) Word8        -- Get the updated Accumulator
        setFlag ZERO (acc == 0)                                                 -- Sets the Zero flag if the result is equal to 0
        setFlag NEGATIVE (testBit acc 7)                                        -- Sets the Negative flag is the result is negative

opASL :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
opASL IMPLICIT = error "Operation ASL does not support IMPLICIT addressing mode"
opASL IMMEDIATE = error "Operation ASL does not support IMMEDIATE addressing mode"
opASL ZEROPAGE_Y = error "Operation ASL does not support ZEROPAGE_Y addressing mode"
opASL RELATIVE = error "Operation ASL does not support RELATIVE addressing mode"
opASL ABSOLUTE_Y = error "Operation ASL does not support ABSOLUTE_Y addressing mode"
opASL INDIRECT = error "Operation ASL does not support INDIRECT addressing mode"
opASL INDEXED_INDIRECT = error "Operation ASL does not support INDEXED_INDIRECT addressing mode"
opASL INDIRECT_INDEXED = error "Operation ASL does not support INDIRECT_INDEXED addressing mode"
opASL ACCUMULATOR = do
        old_acc <- getReg ACC :: AbstractBus a1 => State (MOS6502, a1) Word8    -- Get the Accumulator registers prior to changes
        let carry_flag = testBit old_acc 7                                      -- Carry flag is set to contents of old bit 7
        mapReg ACC ((\x -> shiftL x 1) :: Word8 -> Word8)                       -- Shifts byte one bit to the left
        acc <- getReg ACC :: AbstractBus a1 => State (MOS6502, a1) Word8        -- Get the updated Accumulator
        setFlag CARRY carry_flag                                                -- Sets the Carry flag
        setFlag ZERO (acc == 0)                                                 -- Sets the Zero flag if the result is equal to 0
        setFlag NEGATIVE (testBit acc 7)                                        -- Sets the Negative flag is the result is negative
opASL addr_mode = do
        addr <- getAddr addr_mode                                               -- Get the address given the addressing mode
        byte <- mReadByte addr                                                  -- Read byte from the Bus
        let carry_flag = testBit byte 7                                         -- Carry flag is set to contents of old bit 7
        let new_byte = shiftL byte 1 :: Word8                                   -- Perform the L Shift
        mWriteByte addr new_byte                                                -- Write new byte to same address
        setFlag CARRY carry_flag                                                -- Sets the Carry flag
        setFlag ZERO (new_byte == 0)                                            -- Sets the Zero flag if the result is equal to 0
        setFlag NEGATIVE (testBit new_byte 7)                                   -- Sets the Negative flag is the result is negative

opBCC :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
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
opBCC INDEXED_INDIRECT = error "Operation BCC does not support INDEXED_INDIRECT addressing mode"
opBCC INDIRECT_INDEXED = error "Operation BCC does not support INDIRECT_INDEXED addressing mode"
opBCC RELATIVE = do
        carry_flag <- getFlag CARRY                                             -- Get carry flag
        addr <- getAddr RELATIVE                                                -- Get jump address
        setRegIf (not carry_flag) PC addr                                       -- Jump if Carry flag is clear

opBCS :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
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
opBCS INDEXED_INDIRECT = error "Operation BCS does not support INDEXED_INDIRECT addressing mode"
opBCS INDIRECT_INDEXED = error "Operation BCS does not support INDIRECT_INDEXED addressing mode"
opBCS RELATIVE = do
        carry_flag <- getFlag CARRY                                             -- Get carry flag
        addr <- getAddr RELATIVE                                                -- Get jump address
        setRegIf carry_flag PC addr                                             -- Jump if Carry flag is set

opBEQ :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
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
opBEQ INDEXED_INDIRECT = error "Operation BEQ does not support INDEXED_INDIRECT addressing mode"
opBEQ INDIRECT_INDEXED = error "Operation BEQ does not support INDIRECT_INDEXED addressing mode"
opBEQ RELATIVE = do
        zero_flag <- getFlag ZERO                                               -- Get Zero flag
        addr <- getAddr RELATIVE                                                -- Get jump address
        setRegIf zero_flag PC addr                                              -- Jump if Zero flag is set

opBIT :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
opBIT IMPLICIT = error "Operation BIT does not support IMPLICIT addressing mode"
opBIT ACCUMULATOR = error "Operation BIT does not support ACCUMULATOR addressing mode"
opBIT IMMEDIATE = error "Operation BIT does not support IMMEDIATE addressing mode"
opBIT ZEROPAGE_X = error "Operation BIT does not support ZEROPAGE_X addressing mode"
opBIT ZEROPAGE_Y = error "Operation BIT does not support ZEROPAGE_Y addressing mode"
opBIT RELATIVE = error "Operation BIT does not support RELATIVE addressing mode"
opBIT ABSOLUTE_X = error "Operation BIT does not support ABSOLUTE_X addressing mode"
opBIT ABSOLUTE_Y = error "Operation BIT does not support ABSOLUTE_Y addressing mode"
opBIT INDIRECT = error "Operation BIT does not support INDIRECT addressing mode"
opBIT INDEXED_INDIRECT = error "Operation BIT does not support INDEXED_INDIRECT addressing mode"
opBIT INDIRECT_INDEXED = error "Operation BIT does not support INDIRECT_INDEXED addressing mode"
opBIT addr_mode = do
        addr <- getAddr addr_mode                                               -- Get the address given the addressing mode
        byte <- mReadByte addr                                                  -- Read byte from the Bus
        acc <- getReg ACC :: AbstractBus a1 => State (MOS6502, a1) Word8        -- Get the Accumulator registers
        let and_result = (byte .&. acc)                                         -- Perform AND operation
        setFlag ZERO (and_result == 0)                                          -- Sets the ZERO flag if the result of the AND operation is 0
        setFlag NEGATIVE (b7 byte)                                              -- Sets the Negative flag to the seventh bit of the address value
        setFlag OVERFLOW (b6 byte)                                              -- Sets the Overflow flag to the sixth bit of the address value

opBMI :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
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
opBMI INDEXED_INDIRECT = error "Operation BMI does not support INDEXED_INDIRECT addressing mode"
opBMI INDIRECT_INDEXED = error "Operation BMI does not support INDIRECT_INDEXED addressing mode"
opBMI RELATIVE = do
        negative_flag <- getFlag NEGATIVE                                       -- Get Negative flag
        addr <- getAddr RELATIVE                                                -- Get jump address
        setRegIf negative_flag PC addr                                          -- Jump if Negative flag is set

opBNE :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
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
opBNE INDEXED_INDIRECT = error "Operation BNE does not support INDEXED_INDIRECT addressing mode"
opBNE INDIRECT_INDEXED = error "Operation BNE does not support INDIRECT_INDEXED addressing mode"
opBNE RELATIVE = do
        zero_flag <- getFlag ZERO                                               -- Get Zero flag
        addr <- getAddr RELATIVE                                                -- Get jump address
        setRegIf (not zero_flag) PC addr                                        -- Jump if Zero flag is set

opBPL :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
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
opBPL INDEXED_INDIRECT = error "Operation BPL does not support INDEXED_INDIRECT addressing mode"
opBPL INDIRECT_INDEXED = error "Operation BPL does not support INDIRECT_INDEXED addressing mode"
opBPL RELATIVE = do
        negative_flag <- getFlag NEGATIVE                                       -- Get Zero flag
        addr <- getAddr RELATIVE                                                -- Get jump address
        setRegIf (not negative_flag) PC addr                                    -- Jump if Zero flag is set

opBRK :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
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
opBRK INDEXED_INDIRECT = error "Operation BRK does not support INDEXED_INDIRECT addressing mode"
opBRK INDIRECT_INDEXED = error "Operation BRK does not support INDIRECT_INDEXED addressing mode"
opBRK IMPLICIT = do
        pc <- getReg PC :: AbstractBus a1 => State (MOS6502, a1) Word16         -- Get the PC register
        let pushed_pc = pc + 1  -- Currently, PC points to the byte NEXT to the BRK instruction. But for some ill reason, the 6502 will push the byte after that one to the stack instead.
        let (pchb, pclb) = splitBytes (pushed_pc)                               --
        ps <- getReg PS :: AbstractBus a1 => State (MOS6502, a1) Word8          -- Get the Processor Status register
        mWriteStack pchb                                                        -- Write the high byte of the PC to the stack
        mWriteStack pclb                                                        -- Write the low byte of the PC to the stack
        mWriteStack ps                                                          -- Write the PS to the stack
        irq_lb <- mReadByte 0xFFFE                                              -- Get the IRQ interrupt vector
        irq_hb <- mReadByte 0xFFFF                                              -- 
        let jmp_addr = joinBytes irq_hb irq_lb
        setReg PC jmp_addr                                                      -- Jump to the address
        setFlag BREAK_CMD True                                                  -- Set the Break flag

opBVC :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
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
opBVC INDEXED_INDIRECT = error "Operation BVC does not support INDEXED_INDIRECT addressing mode"
opBVC INDIRECT_INDEXED = error "Operation BVC does not support INDIRECT_INDEXED addressing mode"
opBVC RELATIVE = do
        overflow_flag <- getFlag OVERFLOW                                       -- Get Overflow flag
        addr <- getAddr RELATIVE                                                -- Get jump address
        setRegIf (not overflow_flag) PC addr                                    -- Jump if Zero flag is set

opBVS :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
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
opBVS INDEXED_INDIRECT = error "Operation BVS does not support INDEXED_INDIRECT addressing mode"
opBVS INDIRECT_INDEXED = error "Operation BVS does not support INDIRECT_INDEXED addressing mode"
opBVS RELATIVE = do
        overflow_flag <- getFlag OVERFLOW                                       -- Get Overflow flag
        addr <- getAddr RELATIVE                                                -- Get jump address
        setRegIf overflow_flag PC addr                                          -- Jump if Zero flag is set

opCLC :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
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
opCLC INDEXED_INDIRECT = error "Operation CLC does not support INDEXED_INDIRECT addressing mode"
opCLC INDIRECT_INDEXED = error "Operation CLC does not support INDIRECT_INDEXED addressing mode"
opCLC IMPLICIT = do
        setFlag CARRY False

opCLD :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
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
opCLD INDEXED_INDIRECT = error "Operation CLD does not support INDEXED_INDIRECT addressing mode"
opCLD INDIRECT_INDEXED = error "Operation CLD does not support INDIRECT_INDEXED addressing mode"
opCLD IMPLICIT = do
        setFlag DECIMAL_MODE False

opCLI :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
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
opCLI INDEXED_INDIRECT = error "Operation CLI does not support INDEXED_INDIRECT addressing mode"
opCLI INDIRECT_INDEXED = error "Operation CLI does not support INDIRECT_INDEXED addressing mode"
opCLI IMPLICIT = do
        setFlag INTERRUPT_DISABLE False

opCLV :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
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
opCLV INDEXED_INDIRECT = error "Operation CLV does not support INDEXED_INDIRECT addressing mode"
opCLV INDIRECT_INDEXED = error "Operation CLV does not support INDIRECT_INDEXED addressing mode"
opCLV IMPLICIT = do
        setFlag OVERFLOW False

opCMP :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
opCMP IMPLICIT = error "Operation CMP does not support IMPLICIT addressing mode"
opCMP ACCUMULATOR = error "Operation CMP does not support ACCUMULATOR addressing mode"
opCMP ZEROPAGE_Y = error "Operation CMP does not support ZEROPAGE_Y addressing mode"
opCMP RELATIVE = error "Operation CMP does not support RELATIVE addressing mode"
opCMP INDIRECT = error "Operation CMP does not support INDIRECT addressing mode"
opCMP addr_mode = do
        acc <- getReg ACC :: AbstractBus a1 => State (MOS6502, a1) Word8        -- Get the Accumulator registers prior to changes
        addr <- getAddr addr_mode                                               -- Get the address given the addressing mode
        byte <- mReadByte addr                                                  -- Read byte from the Bus
        let result = acc - byte                                                 -- Compares the accumulator with a memory value
        setFlag ZERO (result == 0)                                              -- Set the Zero flag if they are equal
        setFlag CARRY (b7 result == False)                                      -- Set the Carry flag if Acc >= mem_value
        setFlag NEGATIVE (b7 result == True)                                    -- Set the Negative flag if 

opCPX :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
opCPX IMPLICIT = error "Operation CPX does not support IMPLICIT addressing mode"
opCPX ACCUMULATOR = error "Operation CPX does not support ACCUMULATOR addressing mode"
opCPX ZEROPAGE_X = error "Operation CPX does not support ZEROPAGE_X addressing mode"
opCPX ZEROPAGE_Y = error "Operation CPX does not support ZEROPAGE_Y addressing mode"
opCPX RELATIVE = error "Operation CPX does not support RELATIVE addressing mode"
opCPX ABSOLUTE_X = error "Operation CPX does not support ABSOLUTE_X addressing mode"
opCPX ABSOLUTE_Y = error "Operation CPX does not support ABSOLUTE_Y addressing mode"
opCPX INDIRECT = error "Operation CPX does not support INDIRECT addressing mode"
opCPX INDEXED_INDIRECT = error "Operation CPX does not support INDEXED_INDIRECT addressing mode"
opCPX INDIRECT_INDEXED = error "Operation CPX does not support INDIRECT_INDEXED addressing mode"
opCPX addr_mode = do
        xreg <- getReg IDX :: AbstractBus a1 => State (MOS6502, a1) Word8        -- Get the Accumulator registers prior to changes
        addr <- getAddr addr_mode                                               -- Get the address given the addressing mode
        byte <- mReadByte addr                                                  -- Read byte from the Bus
        let result = xreg - byte                                                 -- Compares the accumulator with a memory value
        setFlag ZERO (result == 0)                                              -- Set the Zero flag if they are equal
        setFlag CARRY (b7 result == False)                                      -- Set the Carry flag if Acc >= mem_value
        setFlag NEGATIVE (b7 result == True)                                    -- Set the Negative flag if 

opCPY :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
opCPY IMPLICIT = error "Operation CPY does not support IMPLICIT addressing mode"
opCPY ACCUMULATOR = error "Operation CPY does not support ACCUMULATOR addressing mode"
opCPY ZEROPAGE_X = error "Operation CPY does not support ZEROPAGE_X addressing mode"
opCPY ZEROPAGE_Y = error "Operation CPY does not support ZEROPAGE_Y addressing mode"
opCPY RELATIVE = error "Operation CPY does not support RELATIVE addressing mode"
opCPY ABSOLUTE_X = error "Operation CPY does not support ABSOLUTE_X addressing mode"
opCPY ABSOLUTE_Y = error "Operation CPY does not support ABSOLUTE_Y addressing mode"
opCPY INDIRECT = error "Operation CPY does not support INDIRECT addressing mode"
opCPY INDEXED_INDIRECT = error "Operation CPY does not support INDEXED_INDIRECT addressing mode"
opCPY INDIRECT_INDEXED = error "Operation CPY does not support INDIRECT_INDEXED addressing mode"
opCPY addr_mode = do
        yreg <- getReg IDY :: AbstractBus a1 => State (MOS6502, a1) Word8        -- Get the Accumulator registers prior to changes
        addr <- getAddr addr_mode                                               -- Get the address given the addressing mode
        byte <- mReadByte addr                                                  -- Read byte from the Bus
        let result = yreg - byte                                                 -- Compares the accumulator with a memory value
        setFlag ZERO (result == 0)                                              -- Set the Zero flag if they are equal
        setFlag CARRY (b7 result == False)                                      -- Set the Carry flag if Acc >= mem_value
        setFlag NEGATIVE (b7 result == True)                                    -- Set the Negative flag if 


opEOR :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
opEOR IMPLICIT = error "Operation EOR does not support IMPLICIT addressing mode"
opEOR ACCUMULATOR = error "Operation EOR does not support ACCUMULATOR addressing mode"
opEOR ZEROPAGE_Y = error "Operation EOR does not support ZEROPAGE_Y addressing mode"
opEOR RELATIVE = error "Operation EOR does not support RELATIVE addressing mode"
opEOR INDIRECT = error "Operation EOR does not support INDIRECT addressing mode"
opEOR addr_mode = do
        old_acc <- getReg ACC :: AbstractBus a1 => State (MOS6502, a1) Word8    -- Get the Accumulator registers prior to changes
        addr <- getAddr addr_mode                                               -- Get the address given the addressing mode
        byte <- mReadByte addr                                                  -- Read byte from the Bus
        mapReg ACC (`xor` byte)                                                 -- XOR the corresponding byte to Accumulator
        acc <- getReg ACC :: AbstractBus a1 => State (MOS6502, a1) Word8        -- Get the updated Accumulator
        setFlag ZERO (acc == 0)                                                 -- Sets the Zero flag if the result is equal to 0
        setFlag NEGATIVE (testBit acc 7)                                        -- Sets the Negative flag is the result is negative

opINX :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
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
opINX INDEXED_INDIRECT = error "Operation INX does not support INDEXED_INDIRECT addressing mode"
opINX INDIRECT_INDEXED = error "Operation INX does not support INDIRECT_INDEXED addressing mode"
opINX IMPLICIT  =  do
        mapReg IDX (+ (1 :: Word8))                                             -- Increases the X Register by one
        idx <- getReg IDX :: AbstractBus a1 => State (MOS6502, a1) Word8        -- Gets the updated X Register
        setFlag ZERO (idx == 0)                                                 -- Sets the Zero flag is the result is equal to 0
        setFlag NEGATIVE (testBit idx 7)                                        -- Sets the Negative flag is the result is negative

opINY :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
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
opINY INDEXED_INDIRECT = error "Operation INY does not support INDEXED_INDIRECT addressing mode"
opINY INDIRECT_INDEXED = error "Operation INY does not support INDIRECT_INDEXED addressing mode"
opINY IMPLICIT  =  do
        mapReg IDY (+ (1 :: Word8))                                             -- Increases the X Register by one
        idy <- getReg IDY :: AbstractBus a1 => State (MOS6502, a1) Word8        -- Gets the updated X Register
        setFlag ZERO (idy == 0)                                                 -- Sets the Zero flag is the result is equal to 0
        setFlag NEGATIVE (testBit idy 7)                                        -- Sets the Negative flag is the result is negative

opLSR :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
opLSR IMPLICIT = error "Operation LSR does not support IMPLICIT addressing mode"
opLSR IMMEDIATE = error "Operation LSR does not support IMMEDIATE addressing mode"
opLSR ZEROPAGE_Y = error "Operation LSR does not support ZEROPAGE_Y addressing mode"
opLSR RELATIVE = error "Operation LSR does not support RELATIVE addressing mode"
opLSR ABSOLUTE_Y = error "Operation LSR does not support ABSOLUTE_Y addressing mode"
opLSR INDIRECT = error "Operation LSR does not support INDIRECT addressing mode"
opLSR INDEXED_INDIRECT = error "Operation LSR does not support INDEXED_INDIRECT addressing mode"
opLSR INDIRECT_INDEXED = error "Operation LSR does not support INDIRECT_INDEXED addressing mode"
opLSR ACCUMULATOR = do
        old_acc <- getReg ACC :: AbstractBus a1 => State (MOS6502, a1) Word8    -- Get the Accumulator registers prior to changes
        let carry_flag = testBit old_acc 0                                      -- Carry flag is set to contents of old bit 0
        mapReg ACC ((\x -> shiftR x 1) :: Word8 -> Word8)                       -- Shifts byte one bit to the right
        acc <- getReg ACC :: AbstractBus a1 => State (MOS6502, a1) Word8        -- Get the updated Accumulator
        setFlag CARRY carry_flag                                                -- Sets the Carry flag
        setFlag ZERO (acc == 0)                                                 -- Sets the Zero flag if the result is equal to 0
        setFlag NEGATIVE (b7 acc)                                               -- Sets the Negative flag is the result is negative
opLSR addr_mode = do
        addr <- getAddr addr_mode                                               -- Get the address given the addressing mode
        byte <- mReadByte addr                                                  -- Read byte from the Bus
        let carry_flag = testBit byte 0                                         -- Carry flag is set to contents of old bit 0
        let new_byte = shiftR byte 1 :: Word8                                   -- Perform the R Shift
        mWriteByte addr new_byte                                                -- Write new byte to same address
        setFlag CARRY carry_flag                                                -- Sets the Carry flag
        setFlag ZERO (new_byte == 0)                                            -- Sets the Zero flag if the result is equal to 0
        setFlag NEGATIVE (b7 new_byte)                                          -- Sets the Negative flag is the result is negative

opORA :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) ()
opORA IMPLICIT = error "Operation ORA does not support IMPLICIT addressing mode"
opORA ACCUMULATOR = error "Operation ORA does not support ACCUMULATOR addressing mode"
opORA ZEROPAGE_Y = error "Operation ORA does not support ZEROPAGE_Y addressing mode"
opORA RELATIVE = error "Operation ORA does not support RELATIVE addressing mode"
opORA INDIRECT = error "Operation ORA does not support INDIRECT addressing mode"
opORA addr_mode = do
        old_acc <- getReg ACC :: AbstractBus a1 => State (MOS6502, a1) Word8    -- Get the Accumulator registers prior to changes
        addr <- getAddr addr_mode                                               -- Get the address given the addressing mode
        byte <- mReadByte addr                                                  -- Read byte from the Bus
        mapReg ACC (.|. byte)                                                   -- OR the corresponding byte to Accumulator
        acc <- getReg ACC :: AbstractBus a1 => State (MOS6502, a1) Word8        -- Get the updated Accumulator
        setFlag ZERO (acc == 0)                                                 -- Sets the Zero flag if the result is equal to 0
        setFlag NEGATIVE (b7 acc)                                               -- Sets the Negative flag is the result is negative
