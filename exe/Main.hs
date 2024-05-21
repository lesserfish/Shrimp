module Main where

import Control.Monad.State
import qualified Shrimp.MOS6502 as MOS
import Shrimp.NES

loop :: StateT NES IO ()
loop = do
    nes <- get
    lift . putStrLn . show . MOS.cLog . MOS.context . cpu $ nes
    lift . putStrLn . show . MOS.mosRegisters . cpu $ nes
    let nes' = execState tick nes
    put nes'
    loop

main :: IO ()
main = do
    nes <- loadNES "/home/lesserfish/Documents/Code/Shrimp/Tools/Roms/nestest.nes"
    putStrLn . show . MOS.mosRegisters . cpu $ nes
    _ <- execStateT loop nes
    return ()
