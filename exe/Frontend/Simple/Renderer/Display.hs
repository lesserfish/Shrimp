module Frontend.Simple.Renderer.Display (new, update) where

import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr, Ptr)
import qualified Data.ByteString as BS
import qualified Shrimp.Display as Display
import Frontend.Simple.Common
import qualified SDL as SDL

update :: Display.Display -> SDL.Texture -> IO()
update display texture = do
    colordata <- Display.toByteString display toColor

    (targetPtr, pitch) <- SDL.lockTexture texture Nothing 
    BS.useAsCString colordata (\ptr -> do
            let sourcePtr = castPtr ptr :: Ptr ()
            copyBytes targetPtr sourcePtr (256 * 240 * 4)
            )
    SDL.unlockTexture texture

new :: SDL.Renderer -> IO SDL.Texture
new renderer = do
    let size = SDL.V2 256 240
    SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStreaming size
