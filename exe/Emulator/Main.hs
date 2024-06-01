module Emulator.Main (
   startLoop,
   loop,
   initializeEmulator
) where

import Data.Word
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

getTCONTROLLER :: StateT EmulatorContext IO (TVar Word8)
getTCONTROLLER = tControllerA . ecPipe <$> get


initializeEmulator :: CommPipe -> IO EmulatorContext
initializeEmulator pipe = do
    now <- getCurrentTime
    return $ EmulatorContext pipe False False now


tick :: StateT EmulatorContext IO ()
tick = do
    tnes <- getTNES 
    nes <- liftIO . atomically $ readTVar tnes
    liftIO $ B.tick nes

fullTick :: StateT EmulatorContext IO ()
fullTick = do
    tnes <- getTNES 
    nes <- liftIO . atomically $ readTVar tnes
    liftIO $ B.fullTick nes

fullFrame :: StateT EmulatorContext IO ()
fullFrame = do
    tnes <- getTNES 
    nes <- liftIO . atomically $ readTVar tnes
    liftIO $ B.fullFrame nes


runNES :: StateT EmulatorContext IO ()
runNES = do
    ectx <- get
    let running = ecRunning ectx
    when running (fullFrame >> (sendFeedback CPUCOMPLETE))

sendFeedback :: Feedback -> StateT EmulatorContext IO ()
sendFeedback feedback = do
    comm <- getETR
    liftIO . atomically $ writeTChan comm feedback

handleCommand :: Command -> StateT EmulatorContext IO ()
handleCommand EXIT =  modify (\ec -> ec{ecExit = True})
handleCommand START = modify (\ex -> ex{ecRunning = True})
handleCommand STOP =  modify (\ex -> ex{ecRunning = False})
handleCommand FRAME = fullFrame >> (sendFeedback CPUCOMPLETE)
handleCommand TICK =  tick >> (sendFeedback CPUCOMPLETE)
handleCommand FULLTICK = fullTick >> (sendFeedback CPUCOMPLETE)

handleCommands :: StateT EmulatorContext IO ()
handleCommands = do
    comm <- getRTE
    command <- liftIO . atomically $ tryReadTChan comm
    case command of
        Nothing -> return ()
        Just cmd -> handleCommand cmd

getExit :: StateT EmulatorContext IO Bool
getExit = ecExit <$> get

updateController :: StateT EmulatorContext IO ()
updateController = do
    tnes <- getTNES
    tcontroller <- getTCONTROLLER
    nes <- liftIO . atomically $ readTVar tnes
    controllerData <- liftIO . atomically $ readTVar tcontroller
    liftIO $ B.setControllerA nes controllerData


loop :: StateT EmulatorContext IO ()
loop = do
    handleCommands
    updateController
    runNES
    exit <- getExit
    if exit then return () else loop
    
startLoop :: EmulatorContext -> IO ()
startLoop ectx = do
    result <- try $ execStateT loop ectx
    case result of
        Left e -> putStrLn $ "ERROR: " ++ show (e :: SomeException)
        Right _ -> return ()
