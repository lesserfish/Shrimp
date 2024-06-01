module Shrimp.Controller where

import Data.Bits
import Data.Word
import Data.IORef
import Shrimp.Utils

data Controller = Controller
    { stateData :: IORef Word8
    , liveData :: IORef Word8
    }

new :: IO Controller
new = do
    stateData <- newIORef 0
    liveData <- newIORef 0
    return $ Controller stateData liveData

readController :: Controller -> IO Word8
readController controller = do
    state <- readIORef (stateData controller)
    let byte = if b7' state then 1 else 0
    let state' = state .<<. 1
    writeIORef (stateData controller) state'
    return byte

writeController :: Controller -> IO ()
writeController controller = do
    newState <- readIORef (liveData controller)
    writeIORef (stateData controller) newState

writeLive :: Controller -> Word8 -> IO ()
writeLive controller byte = writeIORef (liveData controller) byte
