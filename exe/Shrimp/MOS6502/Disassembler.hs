module Shrimp.MOS6502.Disassembler where

import Data.Int
import Data.Word
import Numeric
import Shrimp.MOS6502
import Text.Printf

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

fetchOP :: Word8 -> (String, ADDR_MODE)
fetchOP 0x69 = ("ADC", IMMEDIATE)
fetchOP 0x65 = ("ADC", ZEROPAGE)
fetchOP 0x75 = ("ADC", ZEROPAGE_X)
fetchOP 0x6D = ("ADC", ABSOLUTE)
fetchOP 0x7D = ("ADC", ABSOLUTE_X)
fetchOP 0x79 = ("ADC", ABSOLUTE_Y)
fetchOP 0x61 = ("ADC", INDIRECT_X)
fetchOP 0x71 = ("ADC", INDIRECT_Y)
fetchOP 0x29 = ("AND", IMMEDIATE)
fetchOP 0x25 = ("AND", ZEROPAGE)
fetchOP 0x35 = ("AND", ZEROPAGE_X)
fetchOP 0x2D = ("AND", ABSOLUTE)
fetchOP 0x3D = ("AND", ABSOLUTE_X)
fetchOP 0x39 = ("AND", ABSOLUTE_Y)
fetchOP 0x21 = ("AND", INDIRECT_X)
fetchOP 0x31 = ("AND", INDIRECT_Y)
fetchOP 0x0A = ("ASL", ACCUMULATOR)
fetchOP 0x06 = ("ASL", ZEROPAGE)
fetchOP 0x16 = ("ASL", ZEROPAGE_X)
fetchOP 0x0E = ("ASL", ABSOLUTE)
fetchOP 0x1E = ("ASL", ABSOLUTE_X)
fetchOP 0x90 = ("BCC", RELATIVE)
fetchOP 0xB0 = ("BCS", RELATIVE)
fetchOP 0xF0 = ("BEQ", RELATIVE)
fetchOP 0x24 = ("BIT", ZEROPAGE)
fetchOP 0x2C = ("BIT", ABSOLUTE)
fetchOP 0x30 = ("BMI", RELATIVE)
fetchOP 0xD0 = ("BNE", RELATIVE)
fetchOP 0x10 = ("BPL", RELATIVE)
fetchOP 0x00 = ("BRK", IMPLICIT)
fetchOP 0x50 = ("BVC", RELATIVE)
fetchOP 0x70 = ("BVS", RELATIVE)
fetchOP 0x18 = ("CLC", IMPLICIT)
fetchOP 0xD8 = ("CLD", IMPLICIT)
fetchOP 0x58 = ("CLI", IMPLICIT)
fetchOP 0xB8 = ("CLV", IMPLICIT)
fetchOP 0xC9 = ("CMP", IMMEDIATE)
fetchOP 0xC5 = ("CMP", ZEROPAGE)
fetchOP 0xD5 = ("CMP", ZEROPAGE_X)
fetchOP 0xCD = ("CMP", ABSOLUTE)
fetchOP 0xDD = ("CMP", ABSOLUTE_X)
fetchOP 0xD9 = ("CMP", ABSOLUTE_Y)
fetchOP 0xC1 = ("CMP", INDIRECT_X)
fetchOP 0xD1 = ("CMP", INDIRECT_Y)
fetchOP 0xE0 = ("CPX", IMMEDIATE)
fetchOP 0xE4 = ("CPX", ZEROPAGE)
fetchOP 0xEC = ("CPX", ABSOLUTE)
fetchOP 0xC0 = ("CPY", IMMEDIATE)
fetchOP 0xC4 = ("CPY", ZEROPAGE)
fetchOP 0xCC = ("CPY", ABSOLUTE)
fetchOP 0xC6 = ("DEC", ZEROPAGE)
fetchOP 0xD6 = ("DEC", ZEROPAGE_X)
fetchOP 0xCE = ("DEC", ABSOLUTE)
fetchOP 0xDE = ("DEC", ABSOLUTE_X)
fetchOP 0xCA = ("DEX", IMPLICIT)
fetchOP 0x88 = ("DEY", IMPLICIT)
fetchOP 0x49 = ("EOR", IMMEDIATE)
fetchOP 0x45 = ("EOR", ZEROPAGE)
fetchOP 0x55 = ("EOR", ZEROPAGE_X)
fetchOP 0x4D = ("EOR", ABSOLUTE)
fetchOP 0x5D = ("EOR", ABSOLUTE_X)
fetchOP 0x59 = ("EOR", ABSOLUTE_Y)
fetchOP 0x41 = ("EOR", INDIRECT_X)
fetchOP 0x51 = ("EOR", INDIRECT_Y)
fetchOP 0xE6 = ("INC", ZEROPAGE)
fetchOP 0xF6 = ("INC", ZEROPAGE_X)
fetchOP 0xEE = ("INC", ABSOLUTE)
fetchOP 0xFE = ("INC", ABSOLUTE_X)
fetchOP 0xE8 = ("INX", IMPLICIT)
fetchOP 0xC8 = ("INY", IMPLICIT)
fetchOP 0x4C = ("JMP", ABSOLUTE)
fetchOP 0x6C = ("JMP", INDIRECT)
fetchOP 0x20 = ("JSR", ABSOLUTE)
fetchOP 0xA9 = ("LDA", IMMEDIATE)
fetchOP 0xA5 = ("LDA", ZEROPAGE)
fetchOP 0xB5 = ("LDA", ZEROPAGE_X)
fetchOP 0xAD = ("LDA", ABSOLUTE)
fetchOP 0xBD = ("LDA", ABSOLUTE_X)
fetchOP 0xB9 = ("LDA", ABSOLUTE_Y)
fetchOP 0xA1 = ("LDA", INDIRECT_X)
fetchOP 0xB1 = ("LDA", INDIRECT_Y)
fetchOP 0xA2 = ("LDX", IMMEDIATE)
fetchOP 0xA6 = ("LDX", ZEROPAGE)
fetchOP 0xB6 = ("LDX", ZEROPAGE_Y)
fetchOP 0xAE = ("LDX", ABSOLUTE)
fetchOP 0xBE = ("LDX", ABSOLUTE_Y)
fetchOP 0xA0 = ("LDY", IMMEDIATE)
fetchOP 0xA4 = ("LDY", ZEROPAGE)
fetchOP 0xB4 = ("LDY", ZEROPAGE_X)
fetchOP 0xAC = ("LDY", ABSOLUTE)
fetchOP 0xBC = ("LDY", ABSOLUTE_X)
fetchOP 0x4A = ("LSR", ACCUMULATOR)
fetchOP 0x46 = ("LSR", ZEROPAGE)
fetchOP 0x56 = ("LSR", ZEROPAGE_X)
fetchOP 0x4E = ("LSR", ABSOLUTE)
fetchOP 0x5E = ("LSR", ABSOLUTE_X)
fetchOP 0xEA = ("NOP", IMPLICIT)
fetchOP 0x09 = ("ORA", IMMEDIATE)
fetchOP 0x05 = ("ORA", ZEROPAGE)
fetchOP 0x15 = ("ORA", ZEROPAGE_X)
fetchOP 0x0D = ("ORA", ABSOLUTE)
fetchOP 0x1D = ("ORA", ABSOLUTE_X)
fetchOP 0x19 = ("ORA", ABSOLUTE_Y)
fetchOP 0x01 = ("ORA", INDIRECT_X)
fetchOP 0x11 = ("ORA", INDIRECT_Y)
fetchOP 0x48 = ("PHA", IMPLICIT)
fetchOP 0x08 = ("PHP", IMPLICIT)
fetchOP 0x68 = ("PLA", IMPLICIT)
fetchOP 0x28 = ("PLP", IMPLICIT)
fetchOP 0x2A = ("ROL", ACCUMULATOR)
fetchOP 0x26 = ("ROL", ZEROPAGE)
fetchOP 0x36 = ("ROL", ZEROPAGE_X)
fetchOP 0x2E = ("ROL", ABSOLUTE)
fetchOP 0x3E = ("ROL", ABSOLUTE_X)
fetchOP 0x6A = ("ROR", ACCUMULATOR)
fetchOP 0x66 = ("ROR", ZEROPAGE)
fetchOP 0x76 = ("ROR", ZEROPAGE_X)
fetchOP 0x6E = ("ROR", ABSOLUTE)
fetchOP 0x7E = ("ROR", ABSOLUTE_X)
fetchOP 0x40 = ("RTI", IMPLICIT)
fetchOP 0x60 = ("RTS", IMPLICIT)
fetchOP 0xE9 = ("SBC", IMMEDIATE)
fetchOP 0xE5 = ("SBC", ZEROPAGE)
fetchOP 0xF5 = ("SBC", ZEROPAGE_X)
fetchOP 0xED = ("SBC", ABSOLUTE)
fetchOP 0xFD = ("SBC", ABSOLUTE_X)
fetchOP 0xF9 = ("SBC", ABSOLUTE_Y)
fetchOP 0xE1 = ("SBC", INDIRECT_X)
fetchOP 0xF1 = ("SBC", INDIRECT_Y)
fetchOP 0x38 = ("SEC", IMPLICIT)
fetchOP 0xF8 = ("SED", IMPLICIT)
fetchOP 0x78 = ("SEI", IMPLICIT)
fetchOP 0x85 = ("STA", ZEROPAGE)
fetchOP 0x95 = ("STA", ZEROPAGE_X)
fetchOP 0x8D = ("STA", ABSOLUTE)
fetchOP 0x9D = ("STA", ABSOLUTE_X)
fetchOP 0x99 = ("STA", ABSOLUTE_Y)
fetchOP 0x81 = ("STA", INDIRECT_X)
fetchOP 0x91 = ("STA", INDIRECT_Y)
fetchOP 0x86 = ("STX", ZEROPAGE)
fetchOP 0x96 = ("STX", ZEROPAGE_Y)
fetchOP 0x8E = ("STX", ABSOLUTE)
fetchOP 0x84 = ("STY", ZEROPAGE)
fetchOP 0x94 = ("STY", ZEROPAGE_X)
fetchOP 0x8C = ("STY", ABSOLUTE)
fetchOP 0xAA = ("TAX", IMPLICIT)
fetchOP 0xA8 = ("TAY", IMPLICIT)
fetchOP 0xBA = ("TSX", IMPLICIT)
fetchOP 0x8A = ("TXA", IMPLICIT)
fetchOP 0x9A = ("TXS", IMPLICIT)
fetchOP 0x98 = ("TYA", IMPLICIT)

toHex :: Word8 -> String
toHex w = printf "%02x" w

disassembleArg :: (Monad m) => (Word16 -> m Word8) -> ADDR_MODE -> Word16 -> m (String, Int)
disassembleArg _ IMPLICIT _ = return $ ("", 0)
disassembleArg _ ACCUMULATOR _ = return $ ("A", 0)
disassembleArg read IMMEDIATE addr = do
    argval <- read addr
    return ("#$" ++ toHex argval, 1)
disassembleArg read ZEROPAGE addr = do
    argval <- read addr
    return ("$" ++ toHex argval, 1)
disassembleArg read ZEROPAGE_X addr = do
    argval <- read addr
    return ("$" ++ toHex argval ++ ",X", 1)
disassembleArg read ZEROPAGE_Y addr = do
    argval <- read addr
    return ("$" ++ toHex argval ++ ",Y", 1)
disassembleArg read RELATIVE addr = do
    argval <- read addr
    let offset = (fromIntegral argval :: Int8)
    return ("*" ++ toHex argval ++ " [" ++ (show offset) ++ "]", 1)
disassembleArg read ABSOLUTE addr = do
    a1 <- read addr
    a2 <- read (addr + 1)
    return ("$" ++ toHex a2 ++ toHex a1 ++ "", 2)
disassembleArg read ABSOLUTE_X addr = do
    a1 <- read addr
    a2 <- read (addr + 1)
    return ("$" ++ toHex a2 ++ toHex a1 ++ ",X", 2)
disassembleArg read ABSOLUTE_Y addr = do
    a1 <- read addr
    a2 <- read (addr + 1)
    return ("$" ++ toHex a2 ++ toHex a1 ++ ",Y", 2) -- disassembleArg _ INDIRECT
disassembleArg read INDIRECT addr = do
    a1 <- read addr
    a2 <- read (addr + 1)
    return ("($" ++ toHex a2 ++ toHex a1 ++ ")", 2) -- disassembleArg _ INDIRECT
disassembleArg read INDIRECT_X addr = do
    a <- read addr
    return ("($" ++ toHex a ++ ", X)", 1) -- disassembleArg _ INDIRECT
disassembleArg read INDIRECT_Y addr = do
    a <- read addr
    return ("($" ++ toHex a ++ "), Y", 1) -- disassembleArg _ INDIRECT

disassemble :: (Monad m) => (Word16 -> m Word8) -> Word16 -> m (String, Int)
disassemble read start = do
    opcode <- read start
    let (opname, addr_mode) = fetchOP opcode
    (args, offset) <- disassembleArg read addr_mode (start + 1)
    return $ (opname ++ " " ++ args, 1 + offset)
