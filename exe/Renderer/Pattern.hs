module Renderer.Pattern where

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


getPatternTable :: Int -> NES -> IO [Word8]
getPatternTable pt nes = do
    patternData <- mapM (\addr -> do
        byte <- pPeek addr nes
        return byte) [0..0x1FFF]

    screen <- UMV.replicate (128 * 128) 0 :: IO (UMV.IOVector Word8)
    mapM_ (\(tilex, tiley) -> do
        let tileOffset = 256 * tiley + 16 * tilex    

        mapM_(\line -> do
            let lbs = patternData !! (pt * 0x1000 + tileOffset + line)
            let hbs = patternData !! (pt * 0x1000 + tileOffset + line + 8)

            mapM_(\py -> do
                let xid = tilex * 8 + (7 - py) + pt * 16 * 8
                let yid = tiley * 8 + line
                let memPos = xid + yid * 128

                let pl = if (testBit lbs py) then 2 else 0
                let ph = if (testBit hbs py) then 1 else 0

                let pixel = pl + ph
                UMV.write screen memPos pixel) [0..7]) [0 .. 7])[(x, y) | x <- [0..15], y <- [0 .. 15]]

    fscreen <- UV.freeze screen
    return $ UV.toList fscreen

toColor :: Word8 -> [Word8]
toColor 0 = [255, 255, 255, 255]
toColor 1 = [2, 255, 255, 255]
toColor 2 = [255, 2, 255, 255]
toColor 3 = [255, 255, 2, 255]
toColor _ = [2, 2, 2, 2]

renderPattern :: SDLContext -> SDL.Texture -> NES -> IO ()
renderPattern ctx texture nes = do
    pt <- getPatternTable 0 nes
    let bd = BS.pack . concat . (fmap toColor) $ pt  :: BS.ByteString
    (targetPtr, pitch) <- SDL.lockTexture texture Nothing 
    BS.useAsCString bd (\ptr -> do
            let sourcePtr = castPtr ptr :: Ptr ()
            copyBytes targetPtr sourcePtr (128 * 128 * 4)
        )
    SDL.unlockTexture texture

updatePatternTexture :: (MonadIO m) => SDLContext -> NES -> SDL.Texture -> m()
updatePatternTexture ctx nes texture = do
    liftIO $ renderPattern ctx texture nes

createPatternTexture :: SDLContext -> IO SDL.Texture
createPatternTexture ctx = do
    let renderer = cRenderer ctx
    let size = SDL.V2 128 128
    SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStreaming size
