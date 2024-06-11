module Frontend.Extended.Render (render) where

import Foreign.C.Types (CInt)
import Control.Monad
import Shrimp.NES
import qualified SDL as SDL
import qualified SDL.Font as Font
import Data.Time
import Control.Monad.State
import Frontend.Extended.Common
import qualified Frontend.Extended.Renderer.Display as FRDisplay
import qualified Frontend.Extended.Renderer.Instructions as FRInstructions
import qualified Frontend.Extended.Renderer.Palette as FRPalette
import qualified Frontend.Extended.Renderer.Pattern as FRPattern
import qualified Frontend.Extended.Renderer.Status as FRStatus
import qualified Frontend.Extended.Renderer.Nametable as FRNametable
import qualified Frontend.Extended.Renderer.FPS as FRFPS
import qualified Frontend.Extended.Renderer.OAM as FROAM


getFPS :: StateT RenderContext IO Int
getFPS = do
    rctx <- get
    let before = rsLastRender . rcStatus $ rctx
    now <- liftIO $ getCurrentTime
    let diff = diffUTCTime now before
    let fps = floor $ 1.0 / (realToFrac diff) :: Int
    put rctx{rcStatus = (rcStatus rctx){rsLastRender = now}}
    return fps

update :: StateT RenderContext IO ()
update = do
    nes <- getNES
    rctx <- get
    ctx <- getSDLContext 

    showFPS <- getShowFPS
    fps <- getFPS

    let lDisplayMode = rcLDisplayMode rctx
    let rDisplayMode = rcRDisplayMode rctx


    let fpsTexture     = rtFPS       . rcTextures $ rctx
    let oamTexture     = rtOAM       . rcTextures $ rctx
    let statusTexture  = rtCPUStatus . rcTextures $ rctx
    let paletteTexture = rtPalette   . rcTextures $ rctx
    let patternTexture = rtPattern   . rcTextures $ rctx
    let displayTexture = rtDisplay   . rcTextures $ rctx
    let instructionTexture = rtCPUInstructions . rcTextures $ rctx
    let nametableTexture = rtNametable . rcTextures $ rctx


    liftIO $ FRStatus.update ctx nes statusTexture
    liftIO $ FRPalette.update ctx nes paletteTexture
    when (rDisplayMode == DM_OAM) (liftIO $ FROAM.update ctx nes oamTexture )
    when (rDisplayMode == DM_INSTRUCTION) (liftIO $ FRInstructions.update ctx nes instructionTexture)
    when (rDisplayMode == DM_PATTERN_1) (liftIO $ FRPattern.update ctx nes patternTexture)
    when (rDisplayMode == DM_PATTERN_2) (liftIO $ FRPattern.update ctx nes patternTexture)
    when (lDisplayMode == DM_NAMETABLE_1) (liftIO $ FRNametable.update ctx nes nametableTexture)
    when (lDisplayMode == DM_NAMETABLE_2) (liftIO $ FRNametable.update ctx nes nametableTexture)
    when (lDisplayMode == DM_DISPLAY) (liftIO $ FRDisplay.update ctx nes displayTexture)
    when (showFPS) (liftIO $ FRFPS.update ctx fps fpsTexture)
    setUpdateTextures False

render :: StateT RenderContext IO ()
render = do
    rctx <- get 
    let updateCPU = rsUpdateTextures . rcStatus $ rctx
    let renderer = sdlRenderer . rcSDLContext $ rctx
    when updateCPU update

    SDL.clear renderer

    showFPS <- getShowFPS

    let fpsTexture     = rtFPS       . rcTextures $ rctx
    let oamTexture     = rtOAM       . rcTextures $ rctx
    let statusTexture  = rtCPUStatus . rcTextures $ rctx
    let paletteTexture = rtPalette   . rcTextures $ rctx
    let patternTexture = rtPattern   . rcTextures $ rctx
    let displayTexture = rtDisplay   . rcTextures $ rctx
    let instructionTexture = rtCPUInstructions . rcTextures $ rctx
    let nametableTexture = rtNametable . rcTextures $ rctx

    let lDisplayMode = rcLDisplayMode rctx
    let rDisplayMode = rcRDisplayMode rctx

    SDL.copy renderer statusTexture Nothing (windowSegment (600, 0) (300, 200))
    SDL.copy renderer paletteTexture Nothing (windowSegment (630, 210) (300, 64))
    when (rDisplayMode == DM_OAM) (SDL.copy renderer oamTexture Nothing (windowSegment (600, 300) (300, 350)))
    when (rDisplayMode == DM_INSTRUCTION) (SDL.copy renderer instructionTexture Nothing (windowSegment (600, 300) (300, 350)))
    when (rDisplayMode == DM_PATTERN_1)   (SDL.copy renderer patternTexture (windowSegment (0, 0) (128, 128)) (windowSegment (600, 300) (300, 300)))
    when (rDisplayMode == DM_PATTERN_2)   (SDL.copy renderer patternTexture (windowSegment (0, 128)   (128, 128)) (windowSegment (600, 300) (300, 300)))
    when (lDisplayMode == DM_DISPLAY) (SDL.copy renderer displayTexture Nothing (windowSegment (0, 0) (600, 600)))
    when (lDisplayMode == DM_NAMETABLE_1) (SDL.copy renderer nametableTexture (windowSegment (0, 0) (600, 600)) (windowSegment (0, 0) (600, 600)))
    when (lDisplayMode == DM_NAMETABLE_2) (SDL.copy renderer nametableTexture (windowSegment (600, 0) (600, 600)) (windowSegment (0, 0) (600, 600)))
    when showFPS (SDL.copy renderer fpsTexture Nothing (windowSegment (0, 0) (20, 20)))
    SDL.present renderer
