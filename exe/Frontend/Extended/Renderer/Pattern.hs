module Frontend.Extended.Renderer.Pattern (new, update) where

import Data.Bits
import qualified Shrimp.Cartridge as Cartridge
import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.Vector.Unboxed as UV
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Utils (copyBytes)
import Frontend.Extended.Common
import Shrimp.NES
import qualified Shrimp.BUS as B
import Data.Word
import qualified SDL as SDL

patternColor :: Word8 -> BS.ByteString
patternColor 0 = BS.pack [255, 255, 255, 255]
patternColor 1 = BS.pack [255, 122, 122, 122]
patternColor 2 = BS.pack [255, 80, 80, 80]
patternColor 3 = BS.pack [255, 0, 0, 0]
patternColor _ = BS.pack [255, 0, 0, 0]


getPatternTable :: NES -> IO [Word8]
getPatternTable nes = do
    patternData <- mapM (\addr -> do
        byte <- B.ppuPeek nes addr
        return byte) [0..0x1FFF]

    screen <- UMV.replicate (256 * 128) 0 :: IO (UMV.IOVector Word8)
    mapM_ (\(tilex, tiley, pt) -> do
        let tileOffset = 256 * tiley + 16 * tilex    

        mapM_(\line -> do
            let lbs = patternData !! (pt * 0x1000 + tileOffset + line)
            let hbs = patternData !! (pt * 0x1000 + tileOffset + line + 8)

            mapM_(\py -> do
                let xid = tilex * 8 + (7 - py)
                let yid = tiley * 8 + line
                let memPos = xid + yid * 128 + pt * 128 * 128

                let pl = if (testBit lbs py) then 2 else 0
                let ph = if (testBit hbs py) then 1 else 0

                let pixel = pl + ph
                UMV.write screen memPos pixel) [0..7]) [0 .. 7])[(x, y, pt) | x <- [0..15], y <- [0 .. 15], pt <- [0, 1]]

    fscreen <- UV.freeze screen
    return $ UV.toList fscreen

update :: SDLContext -> NES -> SDL.Texture -> IO ()
update ctx nes texture = do
    table <- getPatternTable nes
    let bd = BS.concat . (fmap patternColor) $ table  :: BS.ByteString
    (targetPtr, pitch) <- SDL.lockTexture texture Nothing 
    BS.useAsCString bd (\ptr -> do
            let sourcePtr = castPtr ptr :: Ptr ()
            copyBytes targetPtr sourcePtr (256 * 128 * 4)
        )
    SDL.unlockTexture texture

new :: SDLContext -> IO SDL.Texture
new ctx = do
    let renderer = sdlRenderer ctx
    let size = SDL.V2 128 256
    SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStreaming size
