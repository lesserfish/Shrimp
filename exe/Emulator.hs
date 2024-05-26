module Emulator where

import Control.Exception
import Control.Monad
import Communication
import Shrimp.NES
import qualified Shrimp.MOS6502 as MOS
import Control.Concurrent.STM
import Control.Monad.State

data EmulatorContext = EmulatorContext
    { ePipe :: CommPipe
    , eExit :: Bool
    , eRunning :: Bool
    }

initializeEmulator :: CommPipe -> IO EmulatorContext
initializeEmulator pipe = return $ EmulatorContext pipe False False

tickNES :: StateT EmulatorContext IO ()
tickNES = do
    ectx <- get
    let pipe = ePipe ectx
    let tnes = tNES pipe
    nes <- liftIO . atomically $ readTVar tnes
    nes' <- liftIO $ execStateT tick nes
    liftIO . atomically $ writeTVar tnes nes'


fullTick :: NES -> IO NES
fullTick nes = do
    let complete = MOS.complete . MOS.context . cpu $ nes
    if complete 
        then return $ setCPUComplete False nes
        else do
            nes' <- execStateT tick nes
            fullTick nes'

fullTickNES :: StateT EmulatorContext IO ()
fullTickNES = do
    ectx <- get
    let pipe = ePipe ectx
    let tnes = tNES pipe
    nes <- liftIO . atomically $ readTVar tnes
    nes' <- liftIO $ fullTick nes
    liftIO . atomically $ writeTVar tnes nes'

runNES :: StateT EmulatorContext IO ()
runNES = do
    ectx <- get
    let running = eRunning ectx
    when running (tickNES >> sendInformation CPUCOMPLETE)

sendInformation :: Information -> StateT EmulatorContext IO ()
sendInformation info = do
    ectx <- get
    let pipe = ePipe ectx
    let comm = etr pipe
    liftIO . atomically $ writeTChan comm info

handleCommand :: Command -> StateT EmulatorContext IO ()
handleCommand EXIT = modify (\ec -> ec{eExit = True})
handleCommand START = modify (\ex -> ex{eRunning = True})
handleCommand STOP = modify (\ex -> ex{eRunning = False})
handleCommand TICK = tickNES >> (sendInformation CPUCOMPLETE)
handleCommand FULLTICK = fullTickNES >> (sendInformation CPUCOMPLETE)

handleCommands :: StateT EmulatorContext IO ()
handleCommands = do
    ectx <- get
    let pipe = ePipe ectx
    let comm = rte pipe
    command <- liftIO . atomically $ tryReadTChan comm
    case command of
        Nothing -> return ()
        Just cmd -> handleCommand cmd

getExit :: StateT EmulatorContext IO Bool
getExit = eExit <$> get

emulationLoop :: StateT EmulatorContext IO ()
emulationLoop = do
    handleCommands
    runNES
    exit <- getExit
    if exit then return () else emulationLoop
    


startEmulationLoop :: EmulatorContext -> IO ()
startEmulationLoop ectx = do
    result <- try $ execStateT emulationLoop ectx
    case result of
        Left e -> putStrLn $ "ERROR: " ++ show (e :: SomeException)
        Right _ -> return ()
