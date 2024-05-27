{-# LANGUAGE OverloadedStrings #-}
module Main where

import Demo
import Control.Concurrent
import Control.Concurrent.STM
import Shrimp.NES
import Communication
import Renderer
import Emulator
import Control.Monad.State
import Renderer.CPUInstructions

emuMain :: IO ()
emuMain = do
    nes <- loadNES "/home/lesserfish/Documents/Code/Shrimp/Tools/Roms/Super_mario_brothers.nes"
    pipe <- createPipe nes
    rctx <- initializeRenderer pipe
    ectx <- initializeEmulator pipe
    _ <- forkIO $ (do
        execStateT rendererLoop rctx
        return ())
    startEmulationLoop ectx
    quitSDL $ rctx

main :: IO ()
main = demoMain
