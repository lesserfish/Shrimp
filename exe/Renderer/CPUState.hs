module Renderer.CPUState where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits
import Data.Text (Text, pack)
import Data.Word
import qualified SDL as SDL
import qualified SDL.Font as Font
import Renderer.Common
import Shrimp.NES
import qualified Shrimp.MOS6502 as MOS
import Text.Printf

renderCPUStatus :: SDLContext -> NES -> IO()
renderCPUStatus ctx nes = do
    let renderData = RenderData (cRenderer ctx) (cStatusFont ctx)
    let ps = MOS.ps . MOS.registers . cpu $ nes :: Word8
    let regcolor b = if b then green else red

    renderString renderData "STATUS: " (5, 1) white
    renderString renderData "N" (85 + 0 * 20 , 1) (regcolor $ testBit ps 7)
    renderString renderData "V" (85 + 1 * 20 , 1) (regcolor $ testBit ps 6)
    renderString renderData "B" (85 + 3 * 20 , 1) (regcolor $ testBit ps 4)
    renderString renderData "N" (85 + 4 * 20 , 1) (regcolor $ testBit ps 3)
    renderString renderData "I" (85 + 5 * 20 , 1) (regcolor $ testBit ps 2)
    renderString renderData "Z" (85 + 6 * 20 , 1) (regcolor $ testBit ps 1)
    renderString renderData "C" (85 + 7 * 20 , 1) (regcolor $ testBit ps 0)

renderCPUCycles :: SDLContext -> NES -> IO()
renderCPUCycles ctx nes = do
    let renderData = RenderData (cRenderer ctx) (cStatusFont ctx)
    let cycles = show . MOS.cycles . cpu $ nes
    let content = "Cycles: " ++ cycles
    renderString renderData content (120, 79) white

renderCPUClock :: SDLContext -> NES -> IO()
renderCPUClock ctx nes = do
    let renderData = RenderData (cRenderer ctx) (cStatusFont ctx)
    let clock = show . nClock $ nes
    let content = "Clock: " ++ clock
    renderString renderData content (5, 40) white

renderCPURegisters :: SDLContext -> NES -> IO ()
renderCPURegisters ctx nes = do
    let renderData = RenderData (cRenderer ctx) (cStatusFont ctx)
    let registers = MOS.registers . cpu $ nes
    let getContent regName regValue = regName ++ ": 0x" ++ regValue

    renderString renderData (getContent "PC" (toHex4 . MOS.pc  $ registers)) (5, 79 + 0 * 25) white
    renderString renderData (getContent "A"  (toHex2 . MOS.acc $ registers)) (5, 79 + 1 * 25) white
    renderString renderData (getContent "X"  (toHex2 . MOS.idx $ registers)) (5, 79 + 2 * 25) white
    renderString renderData (getContent "Y"  (toHex2 . MOS.idy $ registers)) (5, 79 + 3 * 25) white
    renderString renderData (getContent "P"  (toHex2 . MOS.sp  $ registers)) (5, 79 + 4 * 25) white

updateCPUTexture :: (MonadIO m) => SDLContext -> NES -> SDL.Texture -> m()
updateCPUTexture ctx nes texture = do
    let renderer = cRenderer ctx
    SDL.rendererRenderTarget renderer SDL.$= Just texture
    SDL.clear renderer
    liftIO $ renderCPUStatus ctx nes
    liftIO $ renderCPUClock ctx nes
    liftIO $ renderCPUCycles ctx nes
    liftIO $ renderCPURegisters ctx nes
    SDL.rendererRenderTarget renderer SDL.$= Nothing

createCPUTexture :: SDLContext -> IO SDL.Texture
createCPUTexture ctx = do
    let renderer = cRenderer ctx

    let size = SDL.V2 250 300
    SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget size
