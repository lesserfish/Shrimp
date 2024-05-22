module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.Identity
import Control.Monad.State
import Data.List
import Data.Word
import Shrimp.AbstractBus
import qualified Shrimp.MOS6502 as MOS
import Shrimp.NES
import Text.Printf

toHex :: Word16 -> String
toHex w = printf "%04x" w

instance Show NES where
    show nes = output
      where
        addr = MOS.pc . MOS.mosRegisters . cpu $ nes
        Identity (str, offset) = MOS.disassemble addr nes :: Identity (String, Word16)
        output = "Current Instruction: " ++ str ++ "\n\n" ++ (show . MOS.mosRegisters . cpu $ nes)

(<**>) :: (a -> a) -> Int -> (a -> a)
f <**> n
    | n == 1 = f
    | n <= 0 = id
    | otherwise = f . (f <**> (n - 1))

t n = execState tick n

main :: IO ()
main = do
    nes <- loadNES "/home/lesserfish/Documents/Code/Shrimp/Tools/Roms/nestest.nes"
    let l = runIdentity $ MOS.disassembleL 0xc004 0xcfff nes :: [(Word16, String)]
    let a = concat . (intersperse "\n") . (fmap (\(addr, str) -> toHex addr ++ "\t" ++ str)) $ l
    putStrLn $ a
    return ()
