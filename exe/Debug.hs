module Debug (
    main
) where

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Shrimp.NES
import qualified Shrimp.BUS as BUS
import Control.Monad.State

renderNFrames :: Int -> NES -> IO ()
renderNFrames 0 nes = return ()
renderNFrames n nes = do
    BUS.fullFrame nes
    renderNFrames (n - 1) nes

testFrames :: IO () 
testFrames = do
    let iterations = 4000
    before <- getCurrentTime
    nes <- BUS.load "/home/lesserfish/Documents/Code/Shrimp/Tools/Roms/Super_mario_brothers.nes"
    renderNFrames iterations nes
    after <- getCurrentTime
    let diff = diffUTCTime after before
    putStrLn $ "Time: " ++ (show $ diff) ++ "\nIterations: " ++ show iterations ++ "\nFrames per second: " ++ show (fromIntegral iterations / diff) ++ "\n"


main :: IO ()
main = do
    testFrames

