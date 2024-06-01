{-# LANGUAGE OverloadedStrings #-}
module Frontend.Control (control) where

import Frontend.Common
import Data.Time.Clock
import Control.Monad
import Communication
import Shrimp.NES
import Control.Concurrent.STM
import qualified SDL as SDL
import Frontend.Common
import Control.Monad.State


-- Handling Events


exitProgram :: StateT RenderContext IO ()
exitProgram = do
    liftIO . putStrLn $ "Exiting"
    comm <- getRTE
    liftIO . atomically $ writeTChan comm EXIT
    setExit True

toggleEmulation :: StateT RenderContext IO ()
toggleEmulation = do
    rctx <- get
    running <- getRunning
    comm <- getRTE
    let cmd = if running then STOP else START
    liftIO . atomically $ writeTChan comm cmd
    (not <$> getRunning) >>= setRunning

toggleShowFPS :: StateT RenderContext IO ()
toggleShowFPS = (not <$> getShowFPS) >>= setShowFPS

sendTick :: StateT RenderContext IO ()
sendTick = do
    rctx <- get
    comm <- getRTE
    liftIO . atomically $ writeTChan comm TICK

sendFullTick :: StateT RenderContext IO ()
sendFullTick = do
    comm <- getRTE
    liftIO . atomically $ writeTChan comm FULLTICK

sendFrame :: StateT RenderContext IO ()
sendFrame = do
    comm <- getRTE
    liftIO . atomically $ writeTChan comm FRAME


handleFeedback :: Feedback -> StateT RenderContext IO ()
handleFeedback CPUCOMPLETE = setUpdateTextures True

fetchFeedback :: StateT RenderContext IO ()
fetchFeedback = do
    comm <- getETR
    command <- liftIO . atomically $ tryReadTChan comm
    case command of
        Nothing -> return ()
        Just info -> do
            handleFeedback info



modeLeft :: StateT RenderContext IO ()
modeLeft = modify (\rctx -> rctx{rcRDisplayMode = nextMode . rcRDisplayMode $ rctx}) where
    nextMode DM_PATTERN_1 = DM_PATTERN_2
    nextMode DM_PATTERN_2 = DM_INSTRUCTION
    nextMode DM_INSTRUCTION = DM_PATTERN_1
    nextMode _ = DM_PATTERN_1

modeRight :: StateT RenderContext IO ()
modeRight = modify (\rctx -> rctx{rcRDisplayMode = prevMode . rcRDisplayMode $ rctx}) where
    prevMode DM_PATTERN_1 = DM_INSTRUCTION
    prevMode DM_PATTERN_2 = DM_PATTERN_1
    prevMode DM_INSTRUCTION = DM_PATTERN_2
    prevMode _ = DM_PATTERN_1

modeUp :: StateT RenderContext IO ()
modeUp = modify (\rctx -> rctx{rcLDisplayMode = prevMode . rcLDisplayMode $ rctx}) where
    prevMode DM_DISPLAY = DM_NAMETABLE_1
    prevMode DM_NAMETABLE_1 = DM_NAMETABLE_2
    prevMode DM_NAMETABLE_2 = DM_DISPLAY
    prevMode _ = DM_DISPLAY

modeDown :: StateT RenderContext IO ()
modeDown = modify (\rctx -> rctx{rcLDisplayMode = prevMode . rcLDisplayMode $ rctx}) where
    prevMode DM_DISPLAY = DM_NAMETABLE_2
    prevMode DM_NAMETABLE_2 = DM_NAMETABLE_1
    prevMode DM_NAMETABLE_1 = DM_DISPLAY
    prevMode _ = DM_DISPLAY

handleKeydown :: SDL.Keycode -> StateT RenderContext IO ()
handleKeydown SDL.KeycodeQ = exitProgram
handleKeydown SDL.KeycodeSpace = toggleEmulation
handleKeydown SDL.KeycodeP = toggleShowFPS
handleKeydown SDL.KeycodeN = sendTick
handleKeydown SDL.KeycodeC = sendFullTick 
handleKeydown SDL.KeycodeF = sendFrame
handleKeydown SDL.KeycodeRight = modeRight >> (setUpdateTextures True)
handleKeydown SDL.KeycodeLeft = modeLeft >> (setUpdateTextures True)
handleKeydown SDL.KeycodeUp = modeUp >> (setUpdateTextures True)
handleKeydown SDL.KeycodeDown = modeDown >> (setUpdateTextures True)
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
    rctx <- get
    let before = rsLastRender . rcStatus $ rctx
    now <- liftIO $ getCurrentTime
    let diff = diffUTCTime now before
    if diff > (1/60)
        then do
            put rctx{rcStatus = (rcStatus rctx){rsLastRender = now}}
            return True
        else return False

control :: StateT RenderContext IO ()
control = do
    events <- SDL.pollEvents
    mapM_ handleEvents events
    fetchFeedback
