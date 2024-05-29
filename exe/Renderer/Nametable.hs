module Renderer.Nametable where

import Data.List
import Foreign.Marshal.Utils
import Foreign.Ptr
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Control.Monad.State
import Renderer.Common
import Shrimp.NES
import Data.Word
import qualified SDL as SDL
import Shrimp.AbstractBus

getNametableTile :: NES -> Int -> (Int, Int) -> IO Word8
getNametableTile nes nt (nx, ny) = do
    let baseAddr = 0x2000 + nt * 0x400
    let offset = ny * 32 + nx
    let addr = fromIntegral $ baseAddr + offset :: Word16
    byte <- pPeek addr nes
    return byte

renderNametableLine :: SDLContext -> NES -> Int -> Int -> IO ()
renderNametableLine ctx nes nt ny = do
    let renderData = RenderData (cRenderer ctx) (cNametableFont ctx)
    let col = if mod ny 2 == 0 then white else gray
    tiles <- mapM (\nx -> getNametableTile nes nt (nx, ny)) [0..31] :: IO [Word8]
    let content = (toHex2 ny) ++ "  " ++ (concat . (intersperse " ") . (map toHex2) $ tiles)
    renderString renderData content (0, ny * 20 + 20) col
    
renderNametable :: SDLContext -> NES -> Int -> IO()
renderNametable ctx nes nt = do
    let renderData = RenderData (cRenderer ctx) (cNametableFont ctx)
    let content = "    " ++ (concat . (intersperse " ") .(map toHex2) $ [0..0x1F])
    renderString renderData content (0, 0) white

    mapM_ (renderNametableLine ctx nes nt) [0..29]

updateNametableTexture :: (MonadIO m) => SDLContext -> NES -> SDL.Texture -> NTChoice -> m()
updateNametableTexture ctx nes texture ntc = do
    let nt = if ntc then 0 else 1
    let renderer = cRenderer ctx
    SDL.rendererRenderTarget renderer SDL.$= Just texture
    SDL.clear renderer
    liftIO $ renderNametable ctx nes nt
    SDL.rendererRenderTarget renderer SDL.$= Nothing

createNametableTexture :: SDLContext -> IO SDL.Texture
createNametableTexture ctx = do
    let renderer = cRenderer ctx
    let size = SDL.V2 600 600
    SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget size
