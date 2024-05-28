module Renderer.Render where

import Foreign.C.Types (CInt)
import Renderer.Screen
import Renderer.Palette
import Renderer.Common
import Data.Time.Clock
import Control.Monad
import Communication
import Shrimp.NES
import Control.Concurrent.STM
import qualified SDL as SDL
import qualified SDL.Font as Font
import Renderer.Common
import Renderer.CPUInstructions
import Renderer.CPUState
import Control.Monad.State
import Renderer.Nametable (renderNametable, updateNametableTexture)
import Renderer.Pattern (updatePatternTexture)

getNES :: StateT RenderContext IO NES
getNES = do
    rctx <- get
    let pipe = rPipe rctx
    let tnes = tNES pipe
    nes <- liftIO $ atomically . readTVar $ tnes
    return nes


renderTexture :: (SDLContext -> NES -> SDL.Texture -> IO ()) -> (RenderContext -> SDL.Texture) -> StateT RenderContext IO ()
renderTexture updateFunc getter = do
    rctx <- get 
    let ctx = rSDLContext rctx
    nes <- getNES
    let texture = getter rctx
    liftIO $ updateFunc ctx nes texture

renderCPU :: StateT RenderContext IO ()
renderCPU = do
    rctx <- get 
    nes <- getNES
    let ctx = rSDLContext rctx

    liftIO $ updateInstructionTexture ctx nes (rtCPUInstructions rctx)
    liftIO $ updateCPUTexture ctx nes (rtCPUStatus rctx)
    liftIO $ updateNametableTexture ctx nes (rtNametable rctx) (rNChoice rctx)
    liftIO $ updateScreenTexture ctx nes (rtScreen rctx)
    liftIO $ updatePaletteTexture ctx nes (rtPalette rctx)
    --liftIO $ updatePatternTexture ctx nes (rtPattern rctx) (rNChoice rctx)
    put rctx{rUpdateCPU = False}

render :: StateT RenderContext IO ()
render = do
    rctx <- get 
    let updateCPU = rUpdateCPU rctx
    let ctx = rSDLContext rctx
    let renderer = cRenderer ctx
    when updateCPU renderCPU

    SDL.clear renderer

    SDL.copy renderer (rtCPUStatus rctx) Nothing (windowSegment (600, 0) (300, 200))
    SDL.copy renderer (rtPalette rctx) Nothing (windowSegment (630, 210) (300, 64))
    SDL.copy renderer (rtCPUInstructions rctx) Nothing (windowSegment (600, 300) (300, 350))
    SDL.copy renderer (rtScreen rctx) Nothing (windowSegment (0, 0) (600, 600))

    SDL.present renderer
