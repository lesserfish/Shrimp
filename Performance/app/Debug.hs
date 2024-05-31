module Debug where

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Shrimp.NES
import qualified Shrimp.BUS as BUS
import Control.Monad.State

tickNES :: NES -> IO ()
tickNES nes = BUS.tick nes

emulateFrame :: NES -> IO NES
emulateFrame nes = do
   (complete, nes') <- runStateT fetchPPUComplete nes
   if complete 
    then return nes'
    else (tickNES nes') >>= tickNES


renderNFrames :: Int -> NES -> IO NES
renderNFrames 0 nes = return nes
renderNFrames n nes = do
    nes' <- emulateFrame nes
    renderNFrames (n - 1) nes'

demoMain :: IO ()
demoMain = do
    let iterations = 1000000
    before <- getCurrentTime
    nes <- loadNES "/home/lesserfish/Documents/Code/Shrimp/Tools/Roms/Super_mario_brothers.nes"
    nes' <- renderNFrames iterations nes
    putStrLn $ show nes'
    after <- getCurrentTime
    let diff = diffUTCTime after before
    putStrLn $ "Time: " ++ (show $ diff) ++ "\nIterations: " ++ show iterations ++ "\nTicks per second: " ++ show (fromIntegral iterations / diff)
