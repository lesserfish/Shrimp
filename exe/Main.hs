{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Shrimp.NES
import Communication
import Renderer
import Emulator
import Control.Monad.State
import Renderer.CPUInstructions

main :: IO ()
main = do
    nes <- loadNES "/home/lesserfish/Documents/Code/Shrimp/Tools/Roms/nestest.nes"
    pipe <- createPipe nes
    rctx <- initializeRenderer pipe
    ectx <- initializeEmulator pipe
    _ <- forkIO $ startEmulationLoop ectx
    _ <- execStateT rendererLoop rctx
    quitSDL $ rctx
