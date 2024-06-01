module Frontend.Renderer.Nametable (new, update) where

import Data.List
import Frontend.Common
import Shrimp.NES
import Data.Word
import qualified SDL as SDL
import qualified Shrimp.BUS as B
import qualified Shrimp.Memory as Memory
import qualified Shrimp.Cartridge as Cartridge

getNametableTile :: NES -> Int -> (Int, Int) -> IO Word8
getNametableTile nes nt (nx, ny) = do
    let baseAddr = 0x2000 + nt * 0x400
    let offset = ny * 32 + nx
    let addr = fromIntegral $ baseAddr + offset :: Word16
    B.ppuPeek nes addr

renderNametableLine :: SDLContext -> NES -> Int -> Int -> IO ()
renderNametableLine ctx nes nt ny = do
    let renderData = RenderData (sdlRenderer ctx) (sdlNametableFont ctx)
    let xoffset = nt * 600
    let col = if mod ny 2 == 0 then white else gray
    tiles <- mapM (\nx -> getNametableTile nes nt (nx, ny)) [0..31] :: IO [Word8]
    let content = (toHex2 ny) ++ "  " ++ (concat . (intersperse " ") . (map toHex2) $ tiles)
    renderString renderData content (xoffset, ny * 20 + 20) col
    
renderNametable :: SDLContext -> NES -> Int -> IO()
renderNametable ctx nes nt = do
    let xoffset = nt * 600
    let renderData = RenderData (sdlRenderer ctx) (sdlNametableFont ctx)
    let content = "N" ++ show nt ++  "  " ++ (concat . (intersperse " ") .(map toHex2) $ [0..0x1F])
    renderString renderData content (xoffset, 0) white

    mapM_ (renderNametableLine ctx nes nt) [0..29]

update :: SDLContext -> NES -> SDL.Texture -> IO ()
update ctx nes texture = do
    let renderer = sdlRenderer ctx
    SDL.rendererRenderTarget renderer SDL.$= Just texture
    SDL.clear renderer
    renderNametable ctx nes 0
    renderNametable ctx nes 1
    SDL.rendererRenderTarget renderer SDL.$= Nothing

new :: SDLContext -> IO SDL.Texture
new ctx = do
    let renderer = sdlRenderer ctx
    let size = SDL.V2 1200 600
    SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget size
