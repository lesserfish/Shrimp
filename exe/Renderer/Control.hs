{-# LANGUAGE OverloadedStrings #-}
module Renderer.Control where

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


-- Handling Events

exitProgram :: StateT RenderContext IO ()
exitProgram = do
    rctx <- get
    let pipe = rPipe rctx
    let comm = rte pipe
    liftIO . atomically $ writeTChan comm EXIT
    put rctx{rExit = True}

toggleEmulation :: StateT RenderContext IO ()
toggleEmulation = do
    rctx <- get
    let running = rRunning rctx
    let pipe = rPipe rctx
    let comm = rte pipe
    let cmd = if running then STOP else START
    liftIO . atomically $ writeTChan comm cmd
    put rctx{rRunning = not running}

sendTick :: StateT RenderContext IO ()
sendTick = do
    rctx <- get
    let pipe = rPipe rctx
    let comm = rte pipe
    liftIO . atomically $ writeTChan comm TICK

sendFullTick :: StateT RenderContext IO ()
sendFullTick = do
    rctx <- get
    let pipe = rPipe rctx
    let comm = rte pipe
    liftIO . atomically $ writeTChan comm FULLTICK

handleInformation :: Information -> StateT RenderContext IO ()
handleInformation CPUCOMPLETE = modify (\rctx -> rctx{rUpdateCPU = True})

handleInformations :: StateT RenderContext IO ()
handleInformations = do
    rctx <- get
    let pipe = rPipe rctx
    let comm = etr pipe
    command <- liftIO . atomically $ tryReadTChan comm
    case command of
        Nothing -> return ()
        Just info -> do
            handleInformation info

getExit :: StateT RenderContext IO Bool
getExit = rExit <$> get

getNES :: StateT RenderContext IO NES
getNES = do
    rctx <- get
    let pipe = rPipe rctx
    let tnes = tNES pipe
    nes <- liftIO $ atomically . readTVar $ tnes
    return nes

handleKeydown :: SDL.Keycode -> StateT RenderContext IO ()
handleKeydown SDL.KeycodeQ = exitProgram
handleKeydown SDL.KeycodeSpace = toggleEmulation
handleKeydown SDL.KeycodeN = sendTick
handleKeydown SDL.KeycodeC = sendFullTick 
handleKeydown _ = return ()

handleKeyboard :: SDL.KeyboardEventData -> StateT RenderContext IO ()
handleKeyboard ke = do
    if SDL.keyboardEventKeyMotion ke == SDL.Pressed
        then handleKeydown $ SDL.keysymKeycode (SDL.keyboardEventKeysym ke)
        else return ()

handleEvents :: SDL.Event -> StateT RenderContext IO ()
handleEvents e = do
    case SDL.eventPayload e of
        SDL.KeyboardEvent keyboardEvent -> (handleKeyboard keyboardEvent)
        SDL.QuitEvent -> exitProgram
        _ -> return ()
   

frameReady :: StateT RenderContext IO Bool
frameReady = do
    ctx <- get
    let before = rLastTime ctx
    now <- liftIO $ getCurrentTime
    let diff = diffUTCTime now before
    if diff > (1/60)
        then do
            put ctx{rLastTime = now}
            return True
        else return False

control :: StateT RenderContext IO ()
control = do
    events <- SDL.pollEvents
    mapM_ handleEvents events
    handleInformations
