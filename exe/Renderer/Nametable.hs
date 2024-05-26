module Renderer.CPUInstructions where

import Shrimp.AbstractBus
import Data.Word
import Foreign.C.Types (CInt)
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
import qualified SDL.Raw as SDL

getNES :: StateT RenderContext IO NES
getNES = do
    rctx <- get
    let pipe = rPipe rctx
    let tnes = tNES pipe
    nes <- liftIO $ atomically . readTVar $ tnes
    return nes

readPPUMemory :: Word16 -> StateT NES IO Word8
readPPUMemory addr = do
    byte <- mpPeek addr
    return byte

getNametableTile :: NES -> Int -> (Int, Int) -> IO Word8
getNametableTile nes nt (nx, ny) = do
    let baseAddr = 0x2000 + nt * 0x400
    let offset = ny * 32 + nx
    let addr = fromIntegral $ baseAddr + offset :: Word16
    (byte, _) <- runStateT (readPPUMemory addr) nes
    return byte
