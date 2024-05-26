module Renderer.Pattern where

import Data.Bits
import Data.List.Split
import Data.List
import Shrimp.AbstractBus
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as UV
import Control.Monad.State
import Renderer.Common
import Shrimp.NES
import Data.Word
import qualified SDL as SDL
import Data.IntMap.Merge.Lazy (merge)

mergeBytes :: (Word8, Word8) -> [Word8]
mergeBytes (lsb, hsb) = m lsb hsb where
    lb x i = if (testBit x i) then 1 else 0
    hb x i = if (testBit x i) then 2 else 0
    mi l h i = lb l i + hb h i
    m l h = reverse $ map (mi l h) [0..7]

fromRawPT' :: [Word8] -> [Word8]
fromRawPT' [] = []
fromRawPT' (lsb:hsb:rest) = this ++ fromRawPT' rest where
    this = mergeBytes (lsb, hsb) 

index w h n = mod (n * h) (w * h) + (div n w)


type Pixel = Word8
type TileRow = [Pixel]
type Tile = [TileRow]

fixorder :: [Tile] -> [Pixel]
fixorder lst = concat . concat . fmap (concat . transpose) . (chunksOf 2) $ lst

fromRawPT :: [Word8] -> IO [Word8]
fromRawPT lst = do
    tiles <- mapM (\(tx, ty) -> do
        let offset = ty * 256 + tx * 16
        let lines = map (\line -> mergeBytes ((lst !! (offset + line)), (lst !! (offset + line + 8)))) [0.. 7] :: [TileRow]
        return lines) [(x, y) | x <- [0..15], y <- [0..15]] :: IO [Tile]
    return . fixorder $ tiles

doit :: NES -> IO [Word8]
doit nes = do
    patternData <- mapM (\addr -> pPeek addr nes) [0..(0x1000 - 1)] :: IO [Word8]
    let colorData = fromRawPT' patternData
    test <- fromRawPT patternData
    return test

toColor :: Word8 -> BS.ByteString
toColor 0 = BS.pack [000, 000, 000, 000]
toColor 1 = BS.pack [100, 100, 1, 100]
toColor 2 = BS.pack [180, 180, 180, 180]
toColor 3 = BS.pack [2, 220, 220, 220]
toColor _ = BS.pack [255, 5, 255, 255]

renderPattern :: SDLContext -> SDL.Texture -> NES -> IO ()
renderPattern ctx texture nes = do
    patternData <- doit nes
    let colorData = BS.concat $ map toColor patternData :: BS.ByteString
    (targetPtr, pitch) <- SDL.lockTexture texture Nothing 
    BS.useAsCString colorData (\ptr -> do
            let sourcePtr = castPtr ptr :: Ptr ()
            copyBytes targetPtr sourcePtr (128 * 128 * 4)
        )
    SDL.unlockTexture texture

updatePatternTexture :: (MonadIO m) => SDLContext -> NES -> SDL.Texture -> m()
updatePatternTexture ctx nes texture = do
    return ()

createPatternTexture :: SDLContext -> IO SDL.Texture
createPatternTexture ctx = do
    let renderer = cRenderer ctx
    let size = SDL.V2 128 128
    SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStreaming size
