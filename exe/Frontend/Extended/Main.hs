{-# LANGUAGE OverloadedStrings #-}
module Frontend.Extended.Main ( 
    initializeFrontend,
    quit,
    startLoop,
    loop
) where

import Communication
import Data.Time.Clock
import Foreign.C.Types (CInt)
import Control.Monad
import Shrimp.NES
import qualified SDL as SDL
import qualified SDL.Font as Font
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
import Frontend.Extended.Control
import Frontend.Extended.Render

initializeSDL :: IO SDLContext
initializeSDL = do
    SDL.initializeAll
    Font.initialize
    window <- SDL.createWindow "Shrimp" SDL.defaultWindow{SDL.windowInitialSize = SDL.V2 900 600}
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer{SDL.rendererTargetTexture = True}
    font <- Font.load "/home/lesserfish/Documents/Code/Shrimp/Fonts/SpaceMono-Regular.ttf" 15
    font2 <- Font.load "/home/lesserfish/Documents/Code/Shrimp/Fonts/SpaceMono-Regular.ttf" 10
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 10 10 10 10
    return $ SDLContext window renderer font font2

quit :: RenderContext -> IO ()
quit rctx = do
    SDL.destroyWindow . sdlWindow . rcSDLContext $ rctx
    Font.quit


initializeTextures :: SDLContext -> IO RenderTextures
initializeTextures ctx = do
   instructionTexture <- FRInstructions.new ctx
   statusTexture <- FRStatus.new ctx
   displayTexture <- FRDisplay.new ctx
   paletteTexture <- FRPalette.new ctx
   patternTexture <- FRPattern.new ctx
   nametableTexture <- FRNametable.new ctx
   fpsTexture <- FRFPS.new ctx
   oamTexture <- FROAM.new ctx

   return $ RenderTextures 
                { rtCPUStatus = statusTexture
                , rtCPUInstructions = instructionTexture
                , rtPattern = patternTexture
                , rtPalette = paletteTexture
                , rtDisplay = displayTexture
                , rtNametable = nametableTexture
                , rtFPS = fpsTexture
                , rtOAM = oamTexture
                }


initializeFrontend :: CommPipe -> IO RenderContext
initializeFrontend pipe = do
    ctx <- initializeSDL
    textures <- initializeTextures ctx
    now <- getCurrentTime

    let controller = Controller False False False False False False False False

    let rs = RenderStatus
                { rsExit = False
                , rsLastRender = now
                , rsUpdateTextures = True
                , rsRunning = False
                , rsShowFPS = False
                }

    return $ RenderContext
                { rcTextures = textures
                , rcStatus = rs
                , rcCommunicationPipe = pipe
                , rcLDisplayMode = DM_DISPLAY
                , rcRDisplayMode = DM_INSTRUCTION
                , rcSDLContext = ctx
                , rcController = controller
                }

loop :: StateT RenderContext IO ()
loop = do
    control
    render
    exit <- getExit
    if exit then return () else loop

startLoop :: RenderContext -> IO ()
startLoop s = do
    _ <- execStateT loop s
    return ()
