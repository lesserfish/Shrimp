module Frontend.Extended.Renderer.Palette (new, update) where

import qualified Data.ByteString as BS
import Frontend.Extended.Common
import Shrimp.NES
import Data.Word
import qualified SDL as SDL
import qualified Shrimp.Memory as Memory
import qualified Shrimp.BUS as B
import Data.Bits

getPalette :: NES -> Word16 -> IO [Word8]
getPalette nes pl = do
    let c0 = 0x3F00 + pl * 0x04 + 0x00
    let c1 = 0x3F00 + pl * 0x04 + 0x01
    let c2 = 0x3F00 + pl * 0x04 + 0x02
    let c3 = 0x3F00 + pl * 0x04 + 0x03
    let colors = [c0, c1, c2, c3]
    mapM (\addr -> B.ppuPeek nes addr) colors

renderPalette :: SDLContext -> SDL.Texture -> NES -> Word16 -> IO ()
renderPalette ctx texture nes pl = do
    let renderer = sdlRenderer ctx
    colorbytes <- getPalette nes pl :: IO [Word8]
    mapM_ (\id -> do
        let colorbyte = colorbytes !! id
        let color = BS.unpack $ toColor colorbyte
        let r = color !! 3
        let g = color !! 2
        let b = color !! 1
        let a = color !! 0
        let xoffset = 2
        let xstart = (mod pl 4) * (64 + xoffset) + (fromIntegral id * 8)
        let yoffset = 16
        let ystart = (div pl 4) * (8 + yoffset)
        let area = windowSegment (fromIntegral xstart, fromIntegral ystart) (8, 8)
        SDL.rendererDrawColor renderer SDL.$= SDL.V4 r g b a
        SDL.fillRect renderer area
        SDL.rendererDrawColor renderer SDL.$= SDL.V4 10 10 10 10
        ) [0..(length colorbytes - 1)]

update :: SDLContext -> NES -> SDL.Texture -> IO()
update ctx nes texture = do
    let renderer = sdlRenderer ctx
    SDL.rendererRenderTarget renderer SDL.$= Just texture
    SDL.clear renderer
    mapM_ (renderPalette ctx texture nes) [0..7]
    SDL.rendererRenderTarget renderer SDL.$= Nothing

new :: SDLContext -> IO SDL.Texture
new ctx = do
    let renderer = sdlRenderer ctx
    let size = SDL.V2 288 32
    SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget size
