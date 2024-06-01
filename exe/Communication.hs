module Communication (
    Command (..),
    Feedback (..),
    createPipe,
    CommPipe (..)
) where

import Data.Word
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Shrimp.NES

data Command = START | STOP | TICK | FULLTICK | EXIT | FRAME
data Feedback = CPUCOMPLETE

data CommPipe = CommPipe
    { rte :: TChan Command
    , etr :: TChan Feedback
    , tNES :: TVar NES
    , tControllerA :: TVar Word8
    }

createPipe :: NES -> IO CommPipe
createPipe nes = do
    cchan <- atomically newTChan
    ichan <- atomically newTChan
    tnes <- newTVarIO nes
    tcontrollerA <- newTVarIO 0
    return $ CommPipe cchan ichan tnes tcontrollerA
