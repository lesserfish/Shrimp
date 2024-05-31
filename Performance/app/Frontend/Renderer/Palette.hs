module Frontend.Renderer.Palette (new, update) where

import qualified Data.ByteString as BS
import Frontend.Common
import Shrimp.NES
import Data.Word
import qualified SDL as SDL
import qualified Shrimp.Memory as Memory
import qualified Shrimp.BUS as B
import Data.Bits

ppuReadPL' :: Memory.RAM -> Word16 -> IO Word8
ppuReadPL' plram 0x04 = ppuReadPL' plram 0x0
ppuReadPL' plram 0x08 = ppuReadPL' plram 0x0
ppuReadPL' plram 0x0C = ppuReadPL' plram 0x0


ppuReadPL' plram 0x10 = ppuReadPL' plram 0x0
ppuReadPL' plram 0x14 = ppuReadPL' plram 0x4
ppuReadPL' plram 0x18 = ppuReadPL' plram 0x8
ppuReadPL' plram 0x1C = ppuReadPL' plram 0xC
ppuReadPL' plram addr = Memory.readByte plram addr


ppuReadPL :: Memory.RAM -> Word16 -> IO Word8
ppuReadPL plram addr = ppuReadPL' plram (addr .&. 0x1F)


getPalette :: NES -> Word16 -> IO [Word8]
getPalette nes pl = do
    let plram = B.bPLRAM nes
    let c0 = pl * 0x04 + 0x00
    let c1 = pl * 0x04 + 0x01
    let c2 = pl * 0x04 + 0x02
    let c3 = pl * 0x04 + 0x03
    let colors = [c0, c1, c2, c3]
    mapM (\addr -> ppuReadPL plram addr) colors

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
