module Launcher where

import qualified Shrimp.BUS as BUS
import Frontend.Main
import Emulator.Main
import Communication
import Control.Concurrent

main :: IO()
main = do
    nes <- BUS.load "/home/lesserfish/Documents/Code/Shrimp/Tools/Roms/Super_mario_brothers.nes"
    pipe <- createPipe nes
    rctx <- initializeFrontend pipe
    ectx <- initializeEmulator pipe
    _ <- forkIO $ startEmulationLoop ectx
    startRendererLoop rctx
    quitSDL $ rctx
