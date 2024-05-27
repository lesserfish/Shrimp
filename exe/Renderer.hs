{-# LANGUAGE OverloadedStrings #-}
module Renderer where

import Renderer.Pattern
import Renderer.Control
import Renderer.Render
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
import Renderer.Nametable
import Control.Monad.State
import SDL.Raw (getCurrentAudioDriver)

initializeSDL :: IO SDLContext
initializeSDL = do
    SDL.initializeAll
    Font.initialize
    window <- SDL.createWindow "Shrimp" SDL.defaultWindow{SDL.windowInitialSize = SDL.V2 900 600}
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer{SDL.rendererTargetTexture = True}
    font <- Font.load "/home/lesserfish/Documents/Files/Roboto-Light.ttf" 15
    return $ SDLContext window renderer font

quitSDL :: RenderContext -> IO ()
quitSDL ctx = do
    SDL.destroyWindow . cWindow . rSDLContext $ ctx
    Font.quit

initializeRenderer :: CommPipe -> IO RenderContext
initializeRenderer pipe = do
    ctx <- initializeSDL
    statustext <- createCPUTexture ctx
    insttext <- createInstructionTexture ctx
    nttext <- createNametableTexture ctx
    pttext <- createPatternTexture ctx
    now <- getCurrentTime
    return $ RenderContext
        { rSDLContext = ctx
        , rtCPUStatus= statustext
        , rtCPUInstructions = insttext
        , rtNametable = nttext
        , rtPattern = pttext
        , rPipe = pipe
        , rExit = False
        , rRunning = False
        , rUpdateCPU = True
        , rLastTime = now
        , rDisplayMode = DM_CPUSTATUS
        , rNChoice = nt1
        , rPChoice = pt1
        }

rendererLoop :: StateT RenderContext IO ()
rendererLoop = do
    control
    render
    exit <- getExit
    if exit then return () else rendererLoop

startRendererLoop :: RenderContext -> IO ()
startRendererLoop s = do
    _ <- execStateT rendererLoop s
    return ()
