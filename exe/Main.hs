module Main where

import Shrimp.AbstractBus
import Shrimp.Loader
import Shrimp.MOS6502

main :: IO ()
main = do
    putStrLn $ loader
