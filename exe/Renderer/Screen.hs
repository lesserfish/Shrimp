module Renderer.Screen where

import Foreign.Marshal.Utils
import Foreign.Ptr
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Control.Monad.State
import Renderer.Common
import Shrimp.NES
import qualified Shrimp.Display as Display
import Data.Word
import qualified SDL as SDL
import Shrimp.AbstractBus

    
toColor :: Word8 -> BS.ByteString
toColor 0 = BS.pack [255, 255, 255, 255]
toColor 1 = BS.pack [0, 0, 0, 0]
toColor _ = error "bullshit"

renderScreen :: SDLContext -> NES -> SDL.Texture -> IO()
renderScreen ctx nes texture = do
    colordata <- Display.toByteString (nDisplay nes) toColor
    (targetPtr, pitch) <- SDL.lockTexture texture Nothing 

    BS.useAsCString colordata (\ptr -> do
            let sourcePtr = castPtr ptr :: Ptr ()
            copyBytes targetPtr sourcePtr (256 * 240 * 4)
            )
    SDL.unlockTexture texture

updateScreenTexture :: (MonadIO m) => SDLContext -> NES -> SDL.Texture -> m()
updateScreenTexture ctx nes texture = do
    liftIO $ renderScreen ctx nes texture

createScreenTexture :: SDLContext -> IO SDL.Texture
createScreenTexture ctx = do
    let renderer = cRenderer ctx
    let size = SDL.V2 256 240
    SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStreaming size
