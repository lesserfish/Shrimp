module Renderer.Nametable where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Control.Monad.State
import Renderer.Common
import Shrimp.NES
import Data.Word
import qualified SDL as SDL
import Shrimp.AbstractBus

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

renderNametableLine :: SDLContext -> NES -> Int -> Int -> IO ()
renderNametableLine ctx nes nt ny = do
    let renderData = RenderData (cRenderer ctx) (cStatusFont ctx)
    tiles <- mapM (\nx -> getNametableTile nes nt (nx, ny)) [0..31] :: IO [Word8]
    let content = concat . (map toHex2) $ tiles 
    renderString renderData content (0, ny * 20) white
    

demo :: NES -> IO ()
demo nes = do
    ubx <- UV.freeze . nametableRAM $ nes
    let l = UV.toList ubx
    -- putStrLn $ show l
    return ()

renderNametable :: SDLContext -> NES -> IO()
renderNametable ctx nes = do
    let nt = 0
    mapM_ (renderNametableLine ctx nes nt) [0..29]
    demo nes

updateNametableTexture :: (MonadIO m) => SDLContext -> NES -> SDL.Texture -> m()
updateNametableTexture ctx nes texture = do
    let renderer = cRenderer ctx
    SDL.rendererRenderTarget renderer SDL.$= Just texture
    SDL.clear renderer
    liftIO $ renderNametable ctx nes
    SDL.rendererRenderTarget renderer SDL.$= Nothing

createNametableTexture :: SDLContext -> IO SDL.Texture
createNametableTexture ctx = do
    let renderer = cRenderer ctx
    let size = SDL.V2 600 600
    SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget size
