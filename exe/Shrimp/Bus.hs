module Shrimp.Bus where
import Data.Word
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

mapReg :: (RegisterType a) => REGISTER -> (a -> a) -> State MOS6502 ()
mapReg reg func = do
  cpu <- get
  let registers = mosRegisters cpu
      RegisterValue getter = readRegister reg
      currentVal = getter registers
      updatedVal = func currentVal
      registers' = writeRegister reg updatedVal registers
      cpu' = cpu {mosRegisters = registers'}
  put cpu'

setReg :: (RegisterType a) => REGISTER -> a -> State MOS6502 ()
setReg reg value = mapReg reg (\_ -> value)

getReg :: (RegisterType a) => REGISTER -> State MOS6502 a
getReg reg = do
    mos6502 <- get
    let registers = mosRegisters mos6502
    let RegisterValue getter = readRegister reg
    let regval = getter registers
    return regval

getFlag :: FLAG -> MOS6502 -> Bool
getFlag flag = undefined

setFlag :: FLAG -> Bool -> State MOS6502 ()
setFlag flag value = undefined

setFlagIf :: Bool -> FLAG -> Bool -> State MOS6502 ()
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

getBit :: Bits a => a -> Int -> Bool
getBit byte position = (byte .&. (1 `shiftL` position)) /= 0

getAddr :: AbstractBus a => ADDR_MODE -> State (MOS6502, a) Word16

getAddr IMPLICIT = return 0
getAddr ACCUMULATOR = return 0
getAddr IMMEDIATE = do
    (mos6502, bus) <- get
    let addr = pc . mosRegisters $ mos6502
    let mos6502' = mos6502 {mosRegisters = (mosRegisters mos6502){pc = addr + 1}}
    put (mos6502', bus)
    return addr

getAddr ABSOLUTE = do
    (mos6502, bus) <- get
    let cPC = pc . mosRegisters $ mos6502
    let (bus', hb) = readByte cPC bus
    let (bus'', lb) = readByte (cPC + 1) bus'
    let addr = joinBytes hb lb 
    let mos6502' = mos6502 {mosRegisters = (mosRegisters mos6502){pc = addr + 2}}
    put (mos6502, bus'')
    return addr

    
opADC :: AbstractBus a => ADDR_MODE -> (MOS6502, a) -> (MOS6502, a)
opADC addr_mode (mos6502, bus) = (mos6502'', bus'') where
    (addr, (mos6502', bus')) = runState (getAddr addr_mode) (mos6502, bus)  -- Get the address
    (bus'', byte) = readByte addr bus'
    old_acc = acc . mosRegisters $ mos6502                                  -- Get the accumulator value
    mos6502'' = execState ( do
        mapReg ACC (+ byte)                                                 -- Add corresponding byte to Accumulator
        acc <- getReg ACC :: State MOS6502 Word8                            -- Get the updated Accumulator
        setFlag ZERO (acc == 0)                                             -- Sets the Zero flag if the result is equal to 0
        setFlag NEGATIVE (testBit acc 7)                                    -- Sets the Negative flag is the result is negative
        setFlag OVERFLOW (xor (getBit acc 7) (getBit old_acc 7))            -- Sets the Overflow negative if the seventh bit changes
        ) mos6502'

opINX :: AbstractBus a => ADDR_MODE -> (MOS6502, a) -> (MOS6502, a)
opINX IMPLICIT (mos6502, bus) = (mos6502', bus) where
    mos6502' = execState ( do
        mapReg IDX (+ (1 :: Word8))                                         -- Increases the X Register by one
        idx <- getReg IDX :: State MOS6502 Word8                            -- Gets the updated X Register
        setFlag ZERO (idx == 0)                                             -- Sets the Zero flag is the result is equal to 0
        setFlag NEGATIVE (testBit idx 7)                                    -- Sets the Negative flag is the result is negative
        ) mos6502
opINX addr_mode _ = error ("Incompatible addressing mode: INX and " ++ show addr_mode)

