module Renderer.Palette where

import Data.Bits
import Shrimp.AbstractBus
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.Vector.Unboxed as UV
import Control.Monad.State
import Renderer.Common
import Shrimp.NES
import Data.Word
import qualified SDL as SDL


getPalette :: Word16 -> NES -> IO [Word8]
getPalette pl nes = do
    let c0 = 0x3F01 + pl * 0x04 + 0x00
    let c1 = 0x3F01 + pl * 0x04 + 0x01
    let c2 = 0x3F01 + pl * 0x04 + 0x02
    let c3 = 0x3F01 + pl * 0x04 + 0x03
    let colors = [c0, c1, c2, c3]
    mapM (\addr -> pPeek addr nes) colors

renderPalette :: SDLContext -> SDL.Texture -> NES -> Word16 -> IO ()
renderPalette ctx texture nes pl = do
    let renderer = cRenderer ctx
    colorbytes <- getPalette pl nes :: IO [Word8]
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

updatePaletteTexture :: (MonadIO m) => SDLContext -> NES -> SDL.Texture -> m()
updatePaletteTexture ctx nes texture = do
    let renderer = cRenderer ctx
    SDL.rendererRenderTarget renderer SDL.$= Just texture
    SDL.clear renderer
    liftIO $ mapM_ (renderPalette ctx texture nes) [0..7]
    SDL.rendererRenderTarget renderer SDL.$= Nothing

createPaletteTexture :: SDLContext -> IO SDL.Texture
createPaletteTexture ctx = do
    let renderer = cRenderer ctx
    let size = SDL.V2 288 32
    SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget size
