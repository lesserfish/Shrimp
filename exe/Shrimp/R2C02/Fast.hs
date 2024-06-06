module Shrimp.R2C02.Fast where

import Shrimp.Utils
import qualified Shrimp.Display as Display
import Data.Bits
import Control.Monad
import Data.Word
import Control.Monad.State
import Shrimp.R2C02.Internal

-- Tiles are tiles; Sprites are sprites;

-- Helper

toW8 :: (Integral i) => i -> Word8
toW8 = fromIntegral

toW16 :: (Integral i) => i -> Word16
toW16 = fromIntegral

toW32 :: (Integral i) => i -> Word32
toW32 = fromIntegral

toW64 :: (Integral i) => i -> Word64
toW64 = fromIntegral


--
createCheckpoint :: StateT R2C02 IO ()
createCheckpoint = do
    -- Create a save of the current Nametable X, coarse X and fine X
    return ()

resetBuffers :: StateT R2C02 IO ()
resetBuffers = do
    getSpriteBuffer >>= (liftIO . Display.resetLB)
    getBackgroundBuffer >>= (liftIO . Display.resetLB)

mergeBytes :: (Word8, Word8) -> [Word8]
mergeBytes (lsb, msb) = fmap f [0..7] where
    f id = (if testBit lsb id then 0x01 else 0x00) 
         + (if testBit msb id then 0x02 else 0x00)

fetchTileID :: StateT R2C02 IO Word8
fetchTileID = do
    vram <- getVRAM
    let addr = 0x2000 .|. (vram .&. 0x0FFF)
    readByte addr

tileAttribute :: Word16 -> Word16 -> Word8 -> Word8
tileAttribute tx ty byte = fromIntegral output where
    shiftX = if b1 tx then 0x2 else 0x0
    shiftY = if b1 ty then 0x4 else 0x0
    shift = shiftX + shiftY
    output = (byte .>>. shift) .&. 0x03

fetchTileAttribute :: StateT R2C02 IO Word8
fetchTileAttribute = do
    nx <- getVRAMBit L_NAMETABLE_X
    ny <- getVRAMBit L_NAMETABLE_Y
    tx <- toW16 <$> getVRAMData L_COARSE_X
    ty <- toW16 <$> getVRAMData L_COARSE_Y
    let base = 0x23C0 + (nametableBase nx ny) 
    let offset = 8 * (ty .>>. 2) + (tx .>>. 2) :: Word16
    let addr = base + offset
    byte <- readByte addr
    return $ tileAttribute tx ty byte

fetchTileData :: Word16 -> StateT R2C02 IO [Word8]
fetchTileData tileID = do
    ptrn <- getCTRLFlag C_PATTERN_BACKGROUND
    let base = if ptrn then 0x1000 else 0x0000 :: Word16
    fineY <- toW16 <$> (getVRAMData L_FINE_Y)
    let addr = base + tileID * 16 + fineY
    lsb <- readByte (addr + 0x00)
    msb <- readByte (addr + 0x08)
    return $ mergeBytes (lsb, msb)

writeTileToBuffer :: Int -> ([Word8], Word8) -> StateT R2C02 IO ()
writeTileToBuffer tile (tileData, tileAttribute) = do
    bgBuffer <- getBackgroundBuffer
    mapM_ (\x -> do
        let addr = tile * 8 + x
        let pixel = tileData !! x
        let palette = tileAttribute
        let priority = Display.VISIBLE
        liftIO $ Display.setSPixel bgBuffer addr (pixel, palette, priority)
        ) [0..7]

preRenderTile :: Int -> StateT R2C02 IO ()
preRenderTile tile = do
    tileID <- fetchTileID 
    tileAttribute <- fetchTileAttribute
    tileData <- reverse <$> fetchTileData (toW16 tileID)
    writeTileToBuffer tile (tileData, tileAttribute)

preRenderBackground :: StateT R2C02 IO ()
preRenderBackground = do
    createCheckpoint
    resetBuffers
    let tiles = [0..31] --
    mapM_ (\tile -> do
        preRenderTile tile
        incCoarseX ) tiles
    preRenderTile 32 -- Render an additional tile, to accomodate fine x scrolling

toColor :: (Word8, Word8) -> StateT R2C02 IO Word8
toColor (pixel, palette) = do
    let offset = 0x3F00
    let addr = offset + 4 * toW16 palette + toW16 pixel
    byte <- readByte addr
    return byte

render :: StateT R2C02 IO ()
render = do
    cycle <- getCycle
    scanline <- getScanline
    fineX <- getFineX
    let x = cycle + fineX
    bgBuffer <- getBackgroundBuffer
    (pixel, palette, _) <- liftIO $ Display.getSPixel bgBuffer x
    color <- toColor (pixel, palette)
    setPixel (toW16 cycle, toW16 scanline) color

handleVisibleScanline :: StateT R2C02 IO ()
handleVisibleScanline = do
    scanline <- getScanline
    cycle <- getCycle
    when (cycle == 0) preRenderBackground
    when (cycle >= 0 && cycle < 256) render
    when (cycle == 256) incFineY
    when (cycle == 257) transferX

handleEndOfFrame :: StateT R2C02 IO ()
handleEndOfFrame = do
    scanline <- getScanline
    cycle <- getCycle
    when (scanline == 241 && cycle == 1) (do
        setSTATUSFlag S_VERTICAL_BLANK True
        enableNMI <- getCTRLFlag C_ENABLE_NMI
        when enableNMI triggerNMI)

handlePreRender :: StateT R2C02 IO ()
handlePreRender = do
    cycle <- getCycle
    when (cycle == 0) (setSTATUSFlag S_VERTICAL_BLANK False)
    when (cycle == 304) transferY

tick :: StateT R2C02 IO ()
tick = do
    scanline <- getScanline
    cycle <- getCycle
    when (scanline >= 0   && scanline < 240) handleVisibleScanline
    when (scanline >= 241 && scanline < 261) handleEndOfFrame
    when (scanline == 241 && cycle == 0) (setSTATUSFlag S_VERTICAL_BLANK True)
    when (scanline == 261) handlePreRender
    incCycle

tick' :: StateT R2C02 IO (Bool, Bool)
tick' = do
    tick
    done <- fetchComplete
    nmi <- fetchNMI
    return (nmi, done)
