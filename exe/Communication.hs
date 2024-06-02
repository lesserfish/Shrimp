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
    , tControllerA :: TVar Word8
    }

createPipe :: IO CommPipe
createPipe = do
    cchan <- atomically newTChan
    ichan <- atomically newTChan
    tcontrollerA <- newTVarIO 0
    return $ CommPipe cchan ichan tcontrollerA
