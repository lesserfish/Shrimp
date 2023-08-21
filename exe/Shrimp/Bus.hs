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
  readRegister _ = error "Attempted to read Word16 as another type"
  writeRegister PC val regs = regs { pc = val }
  writeRegister _ _ _ = error "Attempted to Write to Word16 with another type"
  
instance RegisterType Word8 where
  readRegister SP = RegisterValue sp
  readRegister ACC = RegisterValue acc
  readRegister IDX = RegisterValue idx
  readRegister IDY = RegisterValue idy
  readRegister PS = RegisterValue ps
  readRegister _ = error "Attempted to read Word8 as another type"
  
  writeRegister SP val regs = regs { sp = val }
  writeRegister ACC val regs = regs { acc = val }
  writeRegister IDX val regs = regs { idx = val }
  writeRegister IDY val regs = regs { idy = val }
  writeRegister PS val regs = regs { ps = val }
  writeRegister _ _ _ = error "Attempted to write Word8 with another type"

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

getFlag :: FLAG -> MOS6502 -> Bool
getFlag flag = undefined

setFlag :: FLAG -> Bool -> State MOS6502 ()
setFlag flag value = do
    cpu <- get
    put cpu

setFlagIf :: Bool -> FLAG -> Bool -> State MOS6502 ()
setFlagIf condition flag value = do
    if condition then (setFlag flag value) else return ()

data Bus = Bus  {    busCPU :: MOS6502
                ,    busRAM :: [Word8]
                } deriving Show

pushCPU :: (MOS6502, Bus) -> Bus
pushCPU (cpu, bus) = bus {busCPU = cpu}

instance AbstractBus Bus where
    writeByte addr byte bus = bus
    readByte addr bus = (bus, 0)

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

getAddr :: AbstractBus a => ADDR_MODE -> a -> MOS6502 -> (a, MOS6502, Word16)
getAddr IMMEDIATE bus mos6502 = (out_bus, out_mos6502, addr) where
    addr = pc . mosRegisters $ mos6502
    out_mos6502 = mos6502 {mosRegisters = (mosRegisters mos6502){pc = addr + 1}}
    out_bus = bus

getAddr ABSOLUTE bus mos6502 = (out_bus, out_mos6502, addr) where
    cPC = pc . mosRegisters $ mos6502
    (bus', hb) = readByte cPC bus
    (bus'', lb) = readByte (cPC + 1) bus'
    addr = joinBytes hb lb 
    mos6502' = mos6502 {mosRegisters = (mosRegisters mos6502){pc = addr + 2}}
    out_mos6502 = mos6502'
    out_bus = bus''

    
opADC :: AbstractBus a => ADDR_MODE -> (MOS6502, a) -> (MOS6502, a)
opADC addr_mode (mos6502, bus) = (mos6502', bus') where
    (bus', mos6502', addr) = getAddr addr_mode bus mos6502
    (bus'', byte) = readByte addr bus'
    cAcc = acc . mosRegisters $ mos6502
    overflow = (toInteger cAcc) + (toInteger byte) > 0xFF :: Bool
    mos6502'' = execState (
                ( setFlagIf (cAcc == 0) ZERO True ) 
            >>  ( setFlagIf overflow OVERFLOW True ) 
            >>  ( mapReg ACC (+ byte) ) 
            >>  ( mapReg PC ((+1) :: Word16 -> Word16))) mos6502   -- Move PC one byte

opINX :: AbstractBus a => ADDR_MODE -> (MOS6502, a) -> (MOS6502, a)
opINX IMPLICIT (mos6502, bus) = (mos6502', bus') where
    cIdx = idx . mosRegisters $ mos6502
    bus' = bus
    mos6502' = execState (
            (setFlagIf (cIdx == 0) ZERO True)                   -- Sets ZERO Flag if IDX == 0
        >>  (setFlagIf (testBit cIdx 6) NEGATIVE True)          -- Sets NEGATIVE Flag is bit 7 of IDX is set
        >>  (mapReg IDX ((+1) :: Word8 -> Word8))) mos6502    -- Adds 1 to IDX in the Registes
opINX addr_mode _ = error ("Incompatible addressing mode: INX and " ++ show addr_mode)

