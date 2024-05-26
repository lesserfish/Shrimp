module Renderer.Render where

import Foreign.C.Types (CInt)
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
import SDL.Raw (getCurrentAudioDriver)
import qualified SDL.Raw as SDL

getNES :: StateT RenderContext IO NES
getNES = do
    rctx <- get
    let pipe = rPipe rctx
    let tnes = tNES pipe
    nes <- liftIO $ atomically . readTVar $ tnes
    return nes


renderInstruction :: StateT RenderContext IO ()
renderInstruction = do
    rctx <- get 
    let ctx = rSDLContext rctx
    let instructionTexture = rtCPUInstructions rctx
    nes <- getNES
    updateInstructionTexture ctx nes instructionTexture
    modify (\ctx -> ctx{rUpdateCPU = False})


renderStatus :: StateT RenderContext IO ()
renderStatus = do
    rctx <- get 
    let ctx = rSDLContext rctx
    let statusTexture = rtCPUStatus rctx
    nes <- getNES
    updateCPUTexture ctx nes statusTexture
    modify (\ctx -> ctx{rUpdateCPU = False})

renderCPU :: StateT RenderContext IO ()
renderCPU = do
    rctx <- get 
    renderStatus
    renderInstruction
    modify (\ctx -> ctx{rUpdateCPU = False})

windowSegment :: (Int, Int) -> (Int, Int) -> Maybe (SDL.Rectangle CInt)
windowSegment (sx, sy) (w, h) = Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral sx) (fromIntegral sy)) (SDL.V2 (fromIntegral w) (fromIntegral h))


render :: StateT RenderContext IO ()
render = do
    rctx <- get 
    let updateCPU = rUpdateCPU rctx
    let ctx = rSDLContext rctx
    let renderer = cRenderer ctx
    let statusTexture = rtCPUStatus rctx
    let instructionTexture = rtCPUInstructions rctx

    SDL.clear renderer
    when updateCPU renderCPU
    SDL.copy renderer statusTexture Nothing (windowSegment (600, 0) (250, 300))
    SDL.copy renderer instructionTexture Nothing (windowSegment (600, 250) (300, 350))
    SDL.present renderer
