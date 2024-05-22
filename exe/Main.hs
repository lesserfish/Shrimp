module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.Identity
import Control.Monad.State
import Data.Word
import Shrimp.AbstractBus
import qualified Shrimp.MOS6502 as MOS
import Shrimp.NES
import System.Console.ANSI

printer :: Int -> NES -> Word16 -> IO ()
printer 0 _ _ = return ()
printer n nes addr = do
    let Identity (str, offset) = MOS.disassemble addr nes :: Identity (String, Word16)
    putStrLn $ str
    printer (n - 1) nes (addr + offset)

main :: IO ()
main = do
    nes <- loadNES "/home/lesserfish/Documents/Code/Shrimp/Tools/Roms/nestest.nes"
    let addr = MOS.pc . MOS.mosRegisters . cpu $ nes
    printer 40 nes addr
    return ()
