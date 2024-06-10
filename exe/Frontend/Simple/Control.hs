{-# LANGUAGE OverloadedStrings #-}
module Frontend.Simple.Control (control) where

import qualified Shrimp.Display as Display
import Frontend.Simple.Common
import Data.Time.Clock
import Control.Monad
import Communication
import Shrimp.NES
import Control.Concurrent.STM
import qualified SDL as SDL
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

clearScreen :: StateT RenderContext IO ()
clearScreen = do
    display <- getDisplay
    liftIO $ Display.reset display


modifyController :: (Controller -> Controller) -> StateT RenderContext IO ()
modifyController f = modify (\rctx -> rctx {rcController = f . rcController $ rctx})

controllerUP :: CKEY -> StateT RenderContext IO ()
controllerUP CUP     = modifyController (\controller -> controller{cUP     = True})
controllerUP CDOWN   = modifyController (\controller -> controller{cDOWN   = True})
controllerUP CLEFT   = modifyController (\controller -> controller{cLEFT   = True})
controllerUP CRIGHT  = modifyController (\controller -> controller{cRIGHT  = True})
controllerUP CSELECT = modifyController (\controller -> controller{cSELECT = True})
controllerUP CSTART  = modifyController (\controller -> controller{cSTART  = True})
controllerUP CA      = modifyController (\controller -> controller{cA      = True})
controllerUP CB      = modifyController (\controller -> controller{cB      = True})

controllerDOWN :: CKEY -> StateT RenderContext IO ()
controllerDOWN CUP     = modifyController (\controller -> controller{cUP     = False})
controllerDOWN CDOWN   = modifyController (\controller -> controller{cDOWN   = False})
controllerDOWN CLEFT   = modifyController (\controller -> controller{cLEFT   = False})
controllerDOWN CRIGHT  = modifyController (\controller -> controller{cRIGHT  = False})
controllerDOWN CSELECT = modifyController (\controller -> controller{cSELECT = False})
controllerDOWN CSTART  = modifyController (\controller -> controller{cSTART  = False})
controllerDOWN CA      = modifyController (\controller -> controller{cA      = False})
controllerDOWN CB      = modifyController (\controller -> controller{cB      = False})

handleKeydown :: SDL.Keycode -> StateT RenderContext IO ()
handleKeydown SDL.KeycodeQ          = exitProgram
handleKeydown SDL.KeycodeSpace      = toggleEmulation
handleKeydown SDL.KeycodeC          = sendTick
handleKeydown SDL.KeycodeT          = sendFullTick 
handleKeydown SDL.KeycodeF          = sendFrame
handleKeydown SDL.KeycodeUp         = controllerUP CUP
handleKeydown SDL.KeycodeDown       = controllerUP CDOWN
handleKeydown SDL.KeycodeRight      = controllerUP CRIGHT
handleKeydown SDL.KeycodeLeft       = controllerUP CLEFT
handleKeydown SDL.KeycodeBackspace  = controllerUP CSELECT
handleKeydown SDL.KeycodeReturn     = controllerUP CSTART
handleKeydown SDL.KeycodeZ          = controllerUP CB
handleKeydown SDL.KeycodeX          = controllerUP CA
handleKeydown SDL.KeycodeR          = clearScreen
handleKeydown _ = return ()


handleKeyup :: SDL.Keycode -> StateT RenderContext IO ()
handleKeyup SDL.KeycodeUp        = controllerDOWN CUP
handleKeyup SDL.KeycodeDown      = controllerDOWN CDOWN
handleKeyup SDL.KeycodeRight     = controllerDOWN CRIGHT
handleKeyup SDL.KeycodeLeft      = controllerDOWN CLEFT
handleKeyup SDL.KeycodeBackspace = controllerDOWN CSELECT
handleKeyup SDL.KeycodeReturn    = controllerDOWN CSTART
handleKeyup SDL.KeycodeZ         = controllerDOWN CB
handleKeyup SDL.KeycodeX         = controllerDOWN CA
handleKeyup _ = return ()



handleKeyboard :: SDL.KeyboardEventData -> StateT RenderContext IO ()
handleKeyboard ke = do
    when (SDL.keyboardEventKeyMotion ke == SDL.Pressed ) (handleKeydown $ SDL.keysymKeycode (SDL.keyboardEventKeysym ke))
    when (SDL.keyboardEventKeyMotion ke == SDL.Released) (handleKeyup $ SDL.keysymKeycode (SDL.keyboardEventKeysym ke))

handleEvents :: SDL.Event -> StateT RenderContext IO ()
handleEvents e = do
    case SDL.eventPayload e of
        SDL.KeyboardEvent keyboardEvent -> (handleKeyboard keyboardEvent)
        SDL.QuitEvent -> exitProgram
        _ -> return ()
   
updateController :: StateT RenderContext IO ()
updateController = do
    tcontroller <- getTCONTROLLER
    controllerData <- getControllerData
    liftIO . atomically $ writeTVar tcontroller controllerData


control :: StateT RenderContext IO ()
control = do
    events <- SDL.pollEvents
    mapM_ handleEvents events
    updateController
