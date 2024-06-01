module Launcher (
    main
) where

import qualified Shrimp.BUS as BUS
import qualified Frontend.Main as F
import qualified Emulator.Main as E
import Communication
import Control.Concurrent

main :: IO()
main = do
    nes <- BUS.load "/home/lesserfish/Documents/Code/Shrimp/Tools/Roms/nestest.nes"
    pipe <- createPipe nes
    rctx <- F.initializeFrontend pipe
    ectx <- E.initializeEmulator pipe
    _ <- forkIO $ E.startLoop ectx
    F.startLoop rctx
    F.quit $ rctx
