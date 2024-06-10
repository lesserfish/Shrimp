module Frontend.Extended.Renderer.Display (new, update, clearScreen) where

import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr, Ptr)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Control.Monad.State
import Frontend.Extended.Common
import Shrimp.NES
import qualified Shrimp.Display as Display
import Data.Word
import qualified SDL as SDL
import qualified Shrimp.BUS as B

clearScreen :: NES -> IO ()
clearScreen nes = do
    Display.reset . B.bDisplay $ nes

update :: SDLContext -> NES -> SDL.Texture -> IO()
update ctx nes texture = do
    colordata <- Display.toByteString (B.bDisplay nes) toColor
    (targetPtr, pitch) <- SDL.lockTexture texture Nothing 

    BS.useAsCString colordata (\ptr -> do
            let sourcePtr = castPtr ptr :: Ptr ()
            copyBytes targetPtr sourcePtr (256 * 240 * 4)
            )
    SDL.unlockTexture texture

new :: SDLContext -> IO SDL.Texture
new ctx = do
    let renderer = sdlRenderer ctx
    let size = SDL.V2 256 240
    SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStreaming size
