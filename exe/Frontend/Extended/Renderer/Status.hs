module Frontend.Extended.Renderer.Status (new, update) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits
import Data.Text (Text, pack)
import Data.Word
import qualified SDL as SDL
import qualified SDL.Font as Font
import Frontend.Extended.Common
import Shrimp.NES
import qualified Shrimp.BUS as B
import qualified Shrimp.MOS6502 as MOS
import Text.Printf
import Data.IORef

renderPS :: SDLContext -> NES -> IO()
renderPS ctx nes = do
    let renderData = RenderData (sdlRenderer ctx) (sdlStatusFont ctx)

    let cpu = B.bCPU nes
    let ps = MOS.ps . MOS.registers $ cpu
    let regcolor b = if b then green else red

    renderString renderData "STATUS: " (5, 1) white
    renderString renderData "N" (85 + 0 * 20 , 1) (regcolor $ testBit ps 7)
    renderString renderData "V" (85 + 1 * 20 , 1) (regcolor $ testBit ps 6)
    renderString renderData "B" (85 + 3 * 20 , 1) (regcolor $ testBit ps 4)
    renderString renderData "N" (85 + 4 * 20 , 1) (regcolor $ testBit ps 3)
    renderString renderData "I" (85 + 5 * 20 , 1) (regcolor $ testBit ps 2)
    renderString renderData "Z" (85 + 6 * 20 , 1) (regcolor $ testBit ps 1)
    renderString renderData "C" (85 + 7 * 20 , 1) (regcolor $ testBit ps 0)

renderCycles :: SDLContext -> NES -> IO()
renderCycles ctx nes = do
    let renderData = RenderData (sdlRenderer ctx) (sdlStatusFont ctx)
    let cpu = B.bCPU nes
    let cycles = show . MOS.cycles $ cpu
    let content = "Cycles: " ++ cycles
    renderString renderData content (120, 79) white

renderClock :: SDLContext -> NES -> IO()
renderClock ctx nes = do
    let renderData = RenderData (sdlRenderer ctx) (sdlStatusFont ctx)
    let cpu = B.bCPU nes
    let clock = show . MOS.clock $ cpu
    let content = "Clock: " ++ clock
    renderString renderData content (5, 40) white

renderRegisters :: SDLContext -> NES -> IO ()
renderRegisters ctx nes = do
    let renderData = RenderData (sdlRenderer ctx) (sdlStatusFont ctx)
    let cpu = B.bCPU nes

    let registers = MOS.registers $ cpu
    let getContent regName regValue = regName ++ ": 0x" ++ regValue

    renderString renderData (getContent "PC" (toHex4 . MOS.pc  $ registers)) (5, 79 + 0 * 25) white
    renderString renderData (getContent "A"  (toHex2 . MOS.acc $ registers)) (5, 79 + 1 * 25) white
    renderString renderData (getContent "X"  (toHex2 . MOS.idx $ registers)) (5, 79 + 2 * 25) white
    renderString renderData (getContent "Y"  (toHex2 . MOS.idy $ registers)) (5, 79 + 3 * 25) white
    renderString renderData (getContent "P"  (toHex2 . MOS.sp  $ registers)) (5, 79 + 4 * 25) white

update :: (MonadIO m) => SDLContext -> NES -> SDL.Texture -> m()
update ctx nes texture = do
    let renderer = sdlRenderer ctx
    SDL.rendererRenderTarget renderer SDL.$= Just texture
    SDL.clear renderer
    liftIO $ renderPS ctx nes
    liftIO $ renderCycles ctx nes
    liftIO $ renderClock ctx nes
    liftIO $ renderRegisters ctx nes
    SDL.rendererRenderTarget renderer SDL.$= Nothing

new :: SDLContext -> IO SDL.Texture
new ctx = do
    let renderer = sdlRenderer ctx
    let size = SDL.V2 300 200
    SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget size
