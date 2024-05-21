module Main where

import Control.Monad.Identity
import Control.Monad.State
import Data.Word
import Shrimp.AbstractBus
import qualified Shrimp.MOS6502 as MOS
import qualified Shrimp.MOS6502.Disassembler as D
import Shrimp.NES

loop :: StateT NES IO ()
loop = do
    nes <- get
    lift . putStrLn . show . MOS.mosRegisters . cpu $ nes
    let nes' = execState tick nes
    put nes'
    loop

helper :: NES -> Word16 -> Identity Word8
helper nes addr = Identity val
  where
    val = pcPeek addr nes

printer :: Int -> (Word16 -> Identity Word8) -> Word16 -> IO ()
printer 0 _ _ = return ()
printer n read addr = do
    let (Identity (str, offset)) = D.disassemble read addr
    putStrLn str
    printer (n - 1) read (addr + (fromIntegral offset))

main :: IO ()
main = do
    nes <- loadNES "/home/lesserfish/Documents/Code/Shrimp/Tools/Roms/nestest.nes"
    putStrLn . show . MOS.mosRegisters . cpu $ nes
    putStrLn "\n"
    let addr = MOS.pc . MOS.mosRegisters . cpu $ nes
    printer 20 (helper nes) addr
    return ()
