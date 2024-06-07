module Shrimp.R2C02.Fast (tick, tick') where

import Shrimp.Utils
import qualified Shrimp.Display as Display
import Data.Bits
import Control.Monad
import Data.Word
import Control.Monad.State
import Shrimp.R2C02.Internal
import qualified Shrimp.MOS6502 as Display


-- Helper

createCheckpoint :: StateT R2C02 IO ()
createCheckpoint = do
    return ()


mergeBytes :: (Word8, Word8) -> [Word8]
mergeBytes (lsb, msb) = fmap f (reverse [0..7]) where
    f id = (if testBit lsb id then 0x01 else 0x00) 
         + (if testBit msb id then 0x02 else 0x00)

-- SPRITES


spriteIsVisible :: Sprite -> StateT R2C02 IO Bool
spriteIsVisible sprite = do
    screenY <- getScanline
    longSprites <- getCTRLFlag C_SPRITE_SIZE
    let spriteHeight = if longSprites then 16 else 8
    let spriteY = toInt $ sprY sprite
    let difference = screenY - spriteY
    return $ (difference >= 0 && difference < spriteHeight)


fetchVisibleSprites' :: Int -> Int -> StateT R2C02 IO [Sprite]
fetchVisibleSprites' _ 9 = return []  -- Stop after 8 visible sprites
fetchVisibleSprites' 64 _ = return [] -- Stop after 64 sprites
fetchVisibleSprites' offset count = do 
    sprite <- getSprite $ offset
    visible <- spriteIsVisible sprite
    if visible
        then do
            rest <- fetchVisibleSprites' (offset + 1) (count + 1)
            return $ [sprite] ++ rest
        else do
            rest <- fetchVisibleSprites' (offset + 1) count
            return rest


-- Fetches a maximum of 8 sprites that are going to be visible next scanline
fetchVisibleSprites :: StateT R2C02 IO [Sprite]
fetchVisibleSprites = do
    sprites <- fetchVisibleSprites' 0 0
    when (length sprites == 9) (setSTATUSFlag S_SPRITE_OVERFLOW True)
    return $ take 8 sprites


fetchSpriteAddress :: Sprite -> StateT R2C02 IO Word16
fetchSpriteAddress sprite = do
    longSprites <- getCTRLFlag C_SPRITE_SIZE 
    if longSprites
        then do
            let spriteID = toW16 . sprTile $ sprite
            let addr = (spriteID .&. 0x01) * 0x1000 + (spriteID .>>. 1) * 0x20
            return addr
        else do
            ptrn <- getCTRLFlag C_PATTERN_SPRITE
            let spriteID = toW16 . sprTile $ sprite
            let base = if ptrn then 0x1000 else 0x0000 :: Word16
            let addr = base + spriteID * 16
            return $ addr


fetchSpriteData :: Sprite -> StateT R2C02 IO [Word8]
fetchSpriteData sprite = do
    screenY <- getScanline
    let spriteY = toInt . sprY $ sprite
    let flipVertical = getSpriteBit SPRITE_VERTICAL_FLIP sprite
    let offset = toW16 $ if flipVertical then 7 - (screenY - spriteY) else (screenY - spriteY)
    base <- fetchSpriteAddress sprite
    lsb <- readByte (base + offset + 0x00)
    msb <- readByte (base + offset + 0x08)
    return $ mergeBytes (lsb, msb)


fetchSpriteAttribute :: Sprite -> Word8
fetchSpriteAttribute sprite =  4 + ((shiftTake1 0 2) . sprAttr $ sprite)


fetchSpritePriority :: Sprite -> Display.Priority
fetchSpritePriority sprite = if (getSpriteBit SPRITE_PRIORITY sprite) then Display.BACK else Display.FRONT


writeToSpriteBuffer :: Int -> ([Word8], Word8, Display.Priority) -> StateT R2C02 IO ()
writeToSpriteBuffer x (spriteData, spriteAttribute, spritePriority) = do
    spriteBuffer <- getSpriteBuffer
    mapM_ (\offset -> do
        let addr = x + offset
        let pixel = spriteData !! offset
        let palette = spriteAttribute
        let priority = if pixel == 0 then Display.BACK else spritePriority

        liftIO $ Display.trySetLBPixel spriteBuffer addr (pixel, palette, priority)
        ) [0..7]

readFromSpriteBuffer :: Int -> StateT R2C02 IO (Word8, Word8, Display.Priority)
readFromSpriteBuffer screenX = do
    spriteBuffer <- getSpriteBuffer
    liftIO $ Display.getLBPixel spriteBuffer screenX

preRenderSprite :: Sprite -> StateT R2C02 IO ()
preRenderSprite sprite = do
    let flipHorizontal = getSpriteBit SPRITE_HORIZONTAL_FLIP sprite
    spriteData <- if flipHorizontal then (reverse <$> fetchSpriteData sprite) else (fetchSpriteData sprite)
    let spriteAttribute = fetchSpriteAttribute sprite
    let spritePriority  = fetchSpritePriority sprite
    let spriteID = sprID sprite

    let x = toInt . sprX $ sprite
    writeToSpriteBuffer x (spriteData, spriteAttribute, spritePriority)
    when (spriteID == 0) (do
        setSprite0X (toInt $ sprX sprite)
        setSprite0Data spriteData
        )

preRenderSprites' :: StateT R2C02 IO ()
preRenderSprites' = do
    sprites <- fetchVisibleSprites
    mapM_ preRenderSprite sprites

preRenderSprites :: StateT R2C02 IO ()
preRenderSprites = do
    getSpriteBuffer >>= (liftIO . Display.resetLB)
    renderSprite <- getMASKFlag M_RENDER_SPRITES
    when renderSprite preRenderSprites'


-- BACKGROUND


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


writeToBGBuffer :: Int -> ([Word8], Word8) -> StateT R2C02 IO ()
writeToBGBuffer tile (tileData, tileAttribute) = do
    bgBuffer <- getBackgroundBuffer
    mapM_ (\x -> do
        let addr = tile * 8 + x
        let pixel = tileData !! x
        let palette = tileAttribute
        let priority = Display.FRONT
        liftIO $ Display.setLBPixel bgBuffer addr (pixel, palette, priority)
        ) [0..7]


readFromBGBuffer :: Int -> StateT R2C02 IO (Word8, Word8)
readFromBGBuffer screenX = do
    fineX <- getFineX
    let x = screenX + fineX
    bgBuffer <- getBackgroundBuffer
    (pixel, palette, _) <- liftIO $ Display.getLBPixel bgBuffer x
    return (pixel, palette)


preRenderTile :: Int -> StateT R2C02 IO ()
preRenderTile tile = do
    tileID <- fetchTileID 
    tileAttribute <- fetchTileAttribute
    tileData <- fetchTileData (toW16 tileID)
    writeToBGBuffer tile (tileData, tileAttribute)


preRenderBackground' :: StateT R2C02 IO ()
preRenderBackground' = do
    let tiles = [0..31] --
    mapM_ (\tile -> do
        preRenderTile tile
        incCoarseX ) tiles
    preRenderTile 32 -- Render an additional tile, to accomodate fine x scrolling


preRenderBackground :: StateT R2C02 IO ()
preRenderBackground = do
    getBackgroundBuffer >>= (liftIO . Display.resetLB)
    renderBackground <- getMASKFlag M_RENDER_BACKGROUND
    when renderBackground preRenderBackground'


checkSprite0Hit :: StateT R2C02 IO ()
checkSprite0Hit = do
    cycle <- getCycle
    let screenX = cycle - 1
    sprite0X <- getSprite0X
    
    let xh = sprite0X - screenX

    let collision = xh >= 0 && xh < 8
    renderBackground <- getMASKFlag M_RENDER_BACKGROUND
    renderSprite <- getMASKFlag M_RENDER_SPRITES
    notSet <- not <$> getSTATUSFlag S_SPRITE_ZERO_HIT

    when (collision && renderBackground && renderSprite && notSet) (do
            sprite0Data <- getSprite0Data
            (bgPixel, bgPalette) <- readFromBGBuffer screenX
            let sprite0Pixel = sprite0Data !! xh
            sprite0Priority <- fetchSpritePriority <$> (getSprite 0)
            sprite0Palette <- fetchSpriteAttribute <$> (getSprite 0)
            let (_, hit) = decidePriority (bgPixel, bgPalette) (sprite0Pixel, sprite0Palette) sprite0Priority
            when hit (setSTATUSFlag S_SPRITE_ZERO_HIT True)
            )

-- PPU Logic


toColor :: (Word8, Word8) -> StateT R2C02 IO Word8
toColor (pixel, palette) = do
    let offset = 0x3F00
    let addr = offset + 4 * toW16 palette + toW16 pixel
    byte <- readByte addr
    return byte


decidePriority :: (Word8, Word8) -> (Word8, Word8) -> Display.Priority -> ((Word8, Word8), Bool)
decidePriority bg _ Display.UNSET = (bg, False)
decidePriority (0, _) (0, _) _ = ((0, 0), False)
decidePriority (0, _) fg _ = (fg, True)
decidePriority bg (0, _) _ = (bg, False)
decidePriority bg fg Display.FRONT = (fg, True)
decidePriority bg fg Display.BACK = (bg, False)


render :: Int -> StateT R2C02 IO ()
render screenX = do
    screenY <- getScanline
    (bgPixel, bgPalette) <- readFromBGBuffer screenX
    (sprPixel, sprPalette, sprPriority) <- readFromSpriteBuffer screenX
    let ((pixel, palette), _) = decidePriority (bgPixel, bgPalette) (sprPixel, sprPalette) sprPriority
    color <- toColor (pixel, palette)
    setPixel (toW16 screenX, toW16 screenY) color


handleVisibleScanline :: StateT R2C02 IO ()
handleVisibleScanline = do
    cycle <- getCycle
    when (cycle == 1) preRenderBackground
    when (cycle == 257) incFineY
    when (cycle == 257) transferX
    when (cycle == 257) preRenderSprites
    when (cycle >= 1 && cycle < 257) (render $ cycle - 1)
    when (cycle >= 1 && cycle < 257) checkSprite0Hit


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
    when (cycle == 1) (setSTATUSFlag S_VERTICAL_BLANK False)
    when (cycle == 1) (setSTATUSFlag S_SPRITE_ZERO_HIT False)
    when (cycle == 1) (setSprite0Data [])
    when (cycle == 1) (setSprite0X (-1))
    when (cycle == 304) transferY

tick :: StateT R2C02 IO ()
tick = do
    scanline <- getScanline
    cycle <- getCycle
    
    when (scanline == (-1)) handlePreRender
    when (scanline == 0 && cycle == 0) incCycle
    when (scanline >= 0   && scanline < 240) handleVisibleScanline
    when (scanline >= 241 && scanline < 260) handleEndOfFrame
    incCycle

tick' :: StateT R2C02 IO (Bool, Bool)
tick' = do
    tick
    done <- fetchComplete
    nmi <- fetchNMI
    return (nmi, done)
