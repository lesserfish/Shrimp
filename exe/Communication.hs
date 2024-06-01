module Communication (
    Command (..),
    Feedback (..),
    createPipe,
    CommPipe (..)
) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Shrimp.NES

data Command = START | STOP | TICK | FULLTICK | EXIT
data Feedback = CPUCOMPLETE

data CommPipe = CommPipe
    { rte :: TChan Command
    , etr :: TChan Feedback
    , tNES :: TVar NES
    }

createPipe :: NES -> IO CommPipe
createPipe nes = do
    cchan <- atomically newTChan
    ichan <- atomically newTChan
    tnes <- newTVarIO nes
    return $ CommPipe cchan ichan tnes
