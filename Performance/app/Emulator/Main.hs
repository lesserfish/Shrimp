module Emulator.Main where

import Data.Time.Clock
import Control.Exception
import Control.Monad
import Communication
import Shrimp.NES
import qualified Shrimp.BUS as B
import Control.Concurrent.STM
import Control.Monad.State

data EmulatorContext = EmulatorContext
    { ecPipe :: CommPipe
    , ecExit :: Bool
    , ecRunning :: Bool
    , rLastTime :: UTCTime
    }

getRTE :: StateT EmulatorContext IO (TChan Command)
getRTE = rte . ecPipe <$> get

getETR :: StateT EmulatorContext IO (TChan Feedback)
getETR = etr . ecPipe <$> get

getTNES :: StateT EmulatorContext IO (TVar NES)
getTNES = tNES . ecPipe <$> get


initializeEmulator :: CommPipe -> IO EmulatorContext
initializeEmulator pipe = do
    now <- getCurrentTime
    return $ EmulatorContext pipe False False now


tickNES :: StateT EmulatorContext IO ()
tickNES = do
    tnes <- getTNES 
    nes <- liftIO . atomically $ readTVar tnes
    liftIO $ B.tick nes

runNES :: StateT EmulatorContext IO ()
runNES = do
    ectx <- get
    let running = ecRunning ectx
    when running (tickNES >> (sendFeedback CPUCOMPLETE))

sendFeedback :: Feedback -> StateT EmulatorContext IO ()
sendFeedback feedback = do
    comm <- getETR
    liftIO . atomically $ writeTChan comm feedback

handleCommand :: Command -> StateT EmulatorContext IO ()
handleCommand EXIT =  modify (\ec -> ec{ecExit = True})
handleCommand START = modify (\ex -> ex{ecRunning = True})
handleCommand STOP =  modify (\ex -> ex{ecRunning = False})
handleCommand TICK =  tickNES >> (sendFeedback CPUCOMPLETE)
handleCommand FULLTICK = tickNES >> (sendFeedback CPUCOMPLETE)

handleCommands :: StateT EmulatorContext IO ()
handleCommands = do
    comm <- getRTE
    command <- liftIO . atomically $ tryReadTChan comm
    case command of
        Nothing -> return ()
        Just cmd -> handleCommand cmd

getExit :: StateT EmulatorContext IO Bool
getExit = ecExit <$> get

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
