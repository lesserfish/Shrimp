{-# LANGUAGE OverloadedStrings #-}
module Frontend.Simple.Main ( 
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
import qualified Shrimp.BUS as Bus
import Control.Monad.State
import Frontend.Simple.Common
import qualified Frontend.Simple.Renderer.Display as FRDisplay
import Frontend.Simple.Control
import Frontend.Simple.Render
import Control.Concurrent.STM
import qualified SDL as SDL

initializeSDL :: IO SDLContext
initializeSDL = do
    SDL.initializeAll
    window <- SDL.createWindow "Shrimp" SDL.defaultWindow{SDL.windowInitialSize = SDL.V2 (2 * 256) (2 * 240)}
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer{SDL.rendererTargetTexture = True}
    return $ SDLContext window renderer

quit :: RenderContext -> IO ()
quit rctx = do
    SDL.destroyWindow . sdlWindow . rcSDLContext $ rctx

initializeFrontend :: CommPipe -> IO RenderContext
initializeFrontend pipe = do
    ctx <- initializeSDL
    screen <- FRDisplay.new (sdlRenderer ctx)
    display <- Bus.bDisplay <$> (liftIO . atomically . readTVar . tNES $ pipe)
    now <- getCurrentTime

    let controller = Controller False False False False False False False False

    let rs = RenderStatus
                { rsExit = False
                , rsLastRender = now
                , rsRunning = False
                }

    return $ RenderContext
                { rcStatus = rs
                , rcCommunicationPipe = pipe
                , rcSDLContext = ctx
                , rcController = controller
                , rcDisplay = display
                , rtScreen = screen
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
