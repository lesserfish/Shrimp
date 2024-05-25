{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Control.Monad.Identity
import Control.Monad.State
import Data.Bits
import Data.Text (Text, pack)
import Data.Word
import SDL (WindowConfig (windowInitialSize))
import qualified SDL as SDL
import qualified SDL.Font as Font
import qualified Shrimp.MOS6502 as MOS
import qualified Shrimp.R2C02 as PPU
import qualified Shrimp.NES as NES
import qualified Shrimp.AbstractBus as BUS
import qualified Shrimp.Memory as Memory
import Text.Printf

data SDLContext = SDLContext
    { sdlWindow :: SDL.Window
    , sdlRenderer :: SDL.Renderer
    , sdlRunning :: Bool
    , sdlFontA :: Font.Font
    , sdlFontB :: Font.Font
    , nes :: !NES.NES
    , sdlUpdateCPUTexture :: Bool
    , sdlUpdatePPUTexture :: Bool
    , cpuTexture :: SDL.Texture
    , ppuTexture :: SDL.Texture
    , nesRunning :: Bool
    }

toHex :: Word8 -> String
toHex w = printf "%02X" w

toHex' :: Word16 -> String
toHex' w = printf "%04X" w

handleKeydown :: SDL.Keycode -> StateT SDLContext IO ()
handleKeydown SDL.KeycodeQ = modify (\ctx -> ctx{sdlRunning = False})
handleKeydown SDL.KeycodeSpace = modify (\ctx -> ctx{nesRunning = not (nesRunning ctx)})
handleKeydown SDL.KeycodeN = do
    ctx <- get
    n' <- liftIO $ execStateT NES.tick (nes ctx)
    put ctx{nes = n', sdlUpdateCPUTexture = True}
handleKeydown SDL.KeycodeC = do
    ctx <- get
    n' <- liftIO $ execStateT NES.tick (nes ctx)
    put ctx{nes = n', sdlUpdateCPUTexture = True}
handleKeydown _ = return ()

handleKeyboard :: SDL.KeyboardEventData -> StateT SDLContext IO ()
handleKeyboard ke = do
    if SDL.keyboardEventKeyMotion ke == SDL.Pressed
        then handleKeydown $ SDL.keysymKeycode (SDL.keyboardEventKeysym ke)
        else return ()

handleEvents :: SDL.Event -> StateT SDLContext IO ()
handleEvents e = do
    case SDL.eventPayload e of
        SDL.KeyboardEvent keyboardEvent -> (handleKeyboard keyboardEvent)
        SDL.QuitEvent -> modify (\ctx -> ctx{sdlRunning = False})
        _ -> return ()

updateCPUStatus' :: Text -> Bool -> (Int, Int) -> StateT SDLContext IO ()
updateCPUStatus' name cond (px, py) = do
    ctx <- get
    let col = if cond then SDL.V4 0 255 0 255 else SDL.V4 255 0 0 255
    (sx, sy) <- Font.size (sdlFontB ctx) name
    surf <- Font.blended (sdlFontB ctx) col name
    text <- SDL.createTextureFromSurface (sdlRenderer ctx) surf
    SDL.copy (sdlRenderer ctx) text Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral px) (fromIntegral py)) (SDL.V2 (fromIntegral sx) (fromIntegral sy)))
    SDL.freeSurface surf
    SDL.destroyTexture text

updateCPUStatus :: StateT SDLContext IO ()
updateCPUStatus = do
    ctx <- get
    let n = nes ctx
    (sx, sy) <- Font.size (sdlFontA ctx) "Status:"
    surf <- Font.blended (sdlFontA ctx) (SDL.V4 255 255 255 255) "Status"
    text <- SDL.createTextureFromSurface (sdlRenderer ctx) surf
    SDL.copy (sdlRenderer ctx) text Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 1) (SDL.V2 (fromIntegral sx) (fromIntegral sy)))
    SDL.freeSurface surf
    SDL.destroyTexture text
    let ps = MOS.ps . MOS.registers . NES.cpu $ n :: Word8
    let h = 7
    let d = 18
    let s = 65
    updateCPUStatus' "N" (testBit ps 7) (s + d * 1, h)
    updateCPUStatus' "V" (testBit ps 6) (s + d * 2, h)
    updateCPUStatus' "B" (testBit ps 4) (s + d * 3, h)
    updateCPUStatus' "D" (testBit ps 3) (s + d * 4, h)
    updateCPUStatus' "I" (testBit ps 2) (s + d * 5, h)
    updateCPUStatus' "Z" (testBit ps 1) (s + d * 6, h)
    updateCPUStatus' "C" (testBit ps 0) (s + d * 7, h)
    return ()

updateCPUCycles :: StateT SDLContext IO ()
updateCPUCycles = do
    ctx <- get
    let n = nes ctx
    let pc = show . MOS.cycles . NES.cpu $ n
    let txt = pack $ "Cycles: " ++ pc
    (sx, sy) <- Font.size (sdlFontB ctx) txt
    surf <- Font.blended (sdlFontB ctx) (SDL.V4 255 255 255 255) txt
    text <- SDL.createTextureFromSurface (sdlRenderer ctx) surf
    SDL.copy (sdlRenderer ctx) text Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 150 60) (SDL.V2 (fromIntegral sx) (fromIntegral sy)))
    SDL.freeSurface surf
    SDL.destroyTexture text

    return ()

updateNESClock :: StateT SDLContext IO ()
updateNESClock = do
    ctx <- get
    let n = nes ctx
    let pc = show . NES.nClock $ n
    let txt = pack $ "Clock: " ++ pc
    (sx, sy) <- Font.size (sdlFontB ctx) txt
    let l = 3 * length pc
    surf <- Font.blended (sdlFontB ctx) (SDL.V4 255 255 255 255) txt
    text <- SDL.createTextureFromSurface (sdlRenderer ctx) surf
    SDL.copy (sdlRenderer ctx) text Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 150 90) (SDL.V2 (fromIntegral sx) (fromIntegral sy)))
    SDL.freeSurface surf
    SDL.destroyTexture text

    return ()

updateCPUPC :: StateT SDLContext IO ()
updateCPUPC = do
    ctx <- get
    let n = nes ctx
    let pc = toHex' . MOS.pc . MOS.registers . NES.cpu $ n
    let txt = pack $ "PC: 0x" ++ pc
    (sx, sy) <- Font.size (sdlFontB ctx) txt
    surf <- Font.blended (sdlFontB ctx) (SDL.V4 255 255 255 255) txt
    text <- SDL.createTextureFromSurface (sdlRenderer ctx) surf
    SDL.copy (sdlRenderer ctx) text Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 60) (SDL.V2 (fromIntegral sx) (fromIntegral sy)))
    SDL.freeSurface surf
    SDL.destroyTexture text

    return ()

updateCPUACC :: StateT SDLContext IO ()
updateCPUACC = do
    ctx <- get
    let n = nes ctx
    let reg = toHex . MOS.acc . MOS.registers . NES.cpu $ n
    let txt = pack $ "A:  0x" ++ reg
    (sx, sy) <- Font.size (sdlFontB ctx) txt
    surf <- Font.blended (sdlFontB ctx) (SDL.V4 255 255 255 255) txt
    text <- SDL.createTextureFromSurface (sdlRenderer ctx) surf
    SDL.copy (sdlRenderer ctx) text Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 90) (SDL.V2 (fromIntegral sx) (fromIntegral sy)))
    SDL.freeSurface surf
    SDL.destroyTexture text

    return ()

updateCPUX :: StateT SDLContext IO ()
updateCPUX = do
    ctx <- get
    let n = nes ctx
    let reg = toHex . MOS.idx . MOS.registers . NES.cpu $ n
    let txt = pack $ "X:  0x" ++ reg
    (sx, sy) <- Font.size (sdlFontB ctx) txt
    surf <- Font.blended (sdlFontB ctx) (SDL.V4 255 255 255 255) txt
    text <- SDL.createTextureFromSurface (sdlRenderer ctx) surf
    SDL.copy (sdlRenderer ctx) text Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 120) (SDL.V2 (fromIntegral sx) (fromIntegral sy)))
    SDL.freeSurface surf
    SDL.destroyTexture text

    return ()

updateCPUY :: StateT SDLContext IO ()
updateCPUY = do
    ctx <- get
    let n = nes ctx
    let reg = toHex . MOS.idy . MOS.registers . NES.cpu $ n
    let txt = pack $ "Y:  0x" ++ reg
    (sx, sy) <- Font.size (sdlFontB ctx) txt
    surf <- Font.blended (sdlFontB ctx) (SDL.V4 255 255 255 255) txt
    text <- SDL.createTextureFromSurface (sdlRenderer ctx) surf
    SDL.copy (sdlRenderer ctx) text Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 150) (SDL.V2 (fromIntegral sx) (fromIntegral sy)))
    SDL.freeSurface surf
    SDL.destroyTexture text

    return ()

updateCPUP :: StateT SDLContext IO ()
updateCPUP = do
    ctx <- get
    let n = nes ctx
    let reg = toHex' . fromIntegral . MOS.sp . MOS.registers . NES.cpu $ n
    let txt = pack $ "P:  0x" ++ reg
    (sx, sy) <- Font.size (sdlFontB ctx) txt
    surf <- Font.blended (sdlFontB ctx) (SDL.V4 255 255 255 255) txt
    text <- SDL.createTextureFromSurface (sdlRenderer ctx) surf
    SDL.copy (sdlRenderer ctx) text Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 180) (SDL.V2 (fromIntegral sx) (fromIntegral sy)))
    SDL.freeSurface surf
    SDL.destroyTexture text

    return ()

updateInstruction :: Bool -> (Word16, String) -> Int -> StateT SDLContext IO ()
updateInstruction sp (addr, inst) py = do
    ctx <- get
    let col = if sp then SDL.V4 0 129 255 255 else SDL.V4 255 255 255 255
    let txt = pack $ "$" ++ toHex' addr ++ ":   " ++ inst
    (sx, sy) <- Font.size (sdlFontB ctx) txt
    surf <- Font.blended (sdlFontB ctx) col txt
    text <- SDL.createTextureFromSurface (sdlRenderer ctx) surf
    SDL.copy (sdlRenderer ctx) text Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 (fromIntegral py)) (SDL.V2 (fromIntegral sx) (fromIntegral sy)))
    SDL.freeSurface surf
    SDL.destroyTexture text

    return ()

updateCPUInstruction' :: Bool -> Int -> Int -> [(Word16, String)] -> StateT SDLContext IO ()
updateCPUInstruction' _ _ _ [] = return ()
updateCPUInstruction' sp starty delta (y : ys) = do
    updateInstruction sp y starty
    updateCPUInstruction' False (starty + delta) delta ys

disassembleL :: Word16 -> Word16 -> StateT NES.NES IO [(Word16, String)]
disassembleL start end = do
    output <- MOS.disassembleL start end ()
    return output

updateCPUInstruction :: StateT SDLContext IO ()
updateCPUInstruction = do
    ctx <- get
    let n = nes ctx
    let pc = MOS.pc . MOS.registers . NES.cpu $ n
    (afterl, _) <- liftIO $ runStateT (disassembleL pc (pc + 20 * 0x04)) n
    let after = take 8 afterl
    (beforel, _) <- liftIO $ runStateT (disassembleL (pc - 20 * 0x04) pc) n
    let before = tail . take 8 . reverse $ beforel
    let offset = 30
    let start = 460
    updateCPUInstruction' True start offset after
    updateCPUInstruction' False (start - offset) (-offset) before
    return ()

updateCPUTexture :: StateT SDLContext IO ()
updateCPUTexture = do
    ctx <- get
    SDL.rendererRenderTarget (sdlRenderer ctx) SDL.$= Just (cpuTexture ctx)
    SDL.clear (sdlRenderer ctx)
    updateCPUStatus
    updateCPUPC
    updateCPUACC
    updateCPUX
    updateCPUY
    updateCPUP
    updateCPUCycles
    updateNESClock
    updateCPUInstruction
    SDL.rendererRenderTarget (sdlRenderer ctx) SDL.$= Nothing

data Pixel = PA | PB | PC | PD deriving Show
data Pixel8 = P8 { p0 :: Pixel
                 , p1 :: Pixel
                 , p2 :: Pixel
                 , p3 :: Pixel
                 , p4 :: Pixel
                 , p5 :: Pixel
                 , p6 :: Pixel
                 , p7 :: Pixel} deriving Show
p82list :: Pixel8 -> [Pixel]
p82list (P8 p0 p1 p2 p3 p4 p5 p6 p7) = [p0, p1, p2, p3, p4, p5, p6, p7]

toPixel :: Bool -> Bool -> Pixel
toPixel False False = PA
toPixel False True = PB
toPixel True False = PC
toPixel True True = PD

combineByte :: (Word8, Word8) -> Pixel8
combineByte (lsb, hsb) = P8 q0 q1 q2 q3 q4 q5 q6 q7 where
    q0 = toPixel (testBit lsb 0) (testBit hsb 0)
    q1 = toPixel (testBit lsb 1) (testBit hsb 1)
    q2 = toPixel (testBit lsb 2) (testBit hsb 2)
    q3 = toPixel (testBit lsb 3) (testBit hsb 3)
    q4 = toPixel (testBit lsb 4) (testBit hsb 4)
    q5 = toPixel (testBit lsb 5) (testBit hsb 5)
    q6 = toPixel (testBit lsb 6) (testBit hsb 6)
    q7 = toPixel (testBit lsb 7) (testBit hsb 7)

getTileOffset :: Int -> (Int, Int) -> Word16
getTileOffset table (x, y) = addr where
    offset = fromIntegral $ y * 16 * 16 + x * 16 :: Word16
    addr = (fromIntegral table) * 0x1000 + offset

peeker :: Word16 -> StateT NES.NES IO Word8
peeker addr = do
    result <- BUS.cPeek addr ()
    return result

getLSB :: NES.NES -> Word16 -> Word16 -> IO Word8
getLSB n offset line = do 
    let addr = offset + line
    (byte, _) <- runStateT (peeker addr) n
    return byte

getHSB :: NES.NES -> Word16 -> Word16 -> IO Word8
getHSB n offset line = do 
    let addr = offset + line + 8
    (byte, _) <- runStateT (peeker addr) n
    return byte

getP8 :: NES.NES -> Word16 -> Word16 -> IO Pixel8
getP8 n offset line = do 
    lsb <- getLSB n offset line
    hsb <- getHSB n offset line
    let pix = combineByte (lsb, hsb)
    return pix

pixel2color PA = SDL.V4 100 100 100 255
pixel2color PB = SDL.V4 155 155 155 255
pixel2color PC = SDL.V4 255 255 200 255
pixel2color PD = SDL.V4 055 055 055 255

renderLine2Surf :: SDL.Surface -> Int -> Pixel8 -> IO ()
renderLine2Surf surf height pix = do
    let pixs = p82list pix
    let pixdata = [(x, px) | x <- [0..7], let px = pixs !! x]
    mapM_ (\(x, px) -> do
            let col = pixel2color px
            SDL.surfaceFillRect surf (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral (7 - x)) (fromIntegral height)) (SDL.V2 1 1)) col
        ) pixdata

getPPUTile :: Int -> (Int, Int) -> StateT SDLContext IO SDL.Texture
getPPUTile pt (tx, ty) = do
    ctx <- get
    surf <- SDL.createRGBSurface (SDL.V2 8 8) SDL.RGBA8888
    mapM_ (\line -> do
        p8 <- liftIO $ getP8 (nes ctx) (getTileOffset pt (tx, ty)) line
        liftIO $ renderLine2Surf surf (fromIntegral line) p8) [0..7]
    text <- SDL.createTextureFromSurface (sdlRenderer ctx) surf
    SDL.freeSurface surf
    return text
    
updatePPUPT :: StateT SDLContext IO ()
updatePPUPT = do
    ctx <- get
    let scale = 2
    let sy = 800 - 128*scale - 30
    let sx = 50
    mapM_ (\(x, y) -> do
        text <- getPPUTile 0 (x, y)
        SDL.copy (sdlRenderer ctx) text Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (sx + fromIntegral x * 8 * scale) (sy + fromIntegral y * 8 * scale)) (SDL.V2 (scale * 8) (scale * 8)))
        SDL.destroyTexture text

        text <- getPPUTile 1 (x, y)
        SDL.copy (sdlRenderer ctx) text Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (sx + 10 + 128 * scale + fromIntegral x * 8 * scale) (sy + fromIntegral y * 8 * scale)) (SDL.V2 (scale * 8) (scale * 8)))
        SDL.destroyTexture text

        ) [(x, y) | x <- [0..15], y <- [0..15]]
    return ()

getNTByte :: NES.NES -> Word16 -> (Word16, Word16) -> IO Word8
getNTByte n nt (nx, ny) = do 
    let base = nt * 0x0400
    let offset = ny * 32 + nx
    let addr = base + offset
    let ram = NES.nametableRAM n
    byte <- liftIO $ Memory.readByteIO ram addr
    return byte
    
updatePPUNT' :: Word16 -> StateT SDLContext IO ()
updatePPUNT' nt = do
    ctx <- get
    let res = 14
    let sx = 85
    let sy = 20 
    mapM_  (\(nx, ny) -> do
            byte <- liftIO $ getNTByte (nes ctx) nt (nx, ny)
            let px = byte .&. 0x0F
            let py = (shiftR byte 4)
            text <- getPPUTile 1 (fromIntegral px, fromIntegral py)
            SDL.copy 
                (sdlRenderer ctx) 
                text 
                Nothing 
                (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral $ sx + nx * res) (fromIntegral $ sy + ny * res)) (SDL.V2 (fromIntegral res) (fromIntegral res)))
            SDL.destroyTexture text
            ) [(x, y) | x <- [0..31], y <- [0..29]]
    return ()

updatePPUNT :: StateT SDLContext IO ()
updatePPUNT = do
    updatePPUNT' 0

updatePPUTexture :: StateT SDLContext IO ()
updatePPUTexture = do
    ctx <- get
    SDL.rendererRenderTarget (sdlRenderer ctx) SDL.$= Just (ppuTexture ctx)
    SDL.clear (sdlRenderer ctx)
    updatePPUPT
    updatePPUNT
    SDL.rendererRenderTarget (sdlRenderer ctx) SDL.$= Nothing

updateTextures :: StateT SDLContext IO ()
updateTextures = do
    ctx <- get
    when (sdlUpdateCPUTexture ctx) updateCPUTexture
    when (sdlUpdatePPUTexture ctx) updatePPUTexture
    put ctx{sdlUpdateCPUTexture = False, sdlUpdatePPUTexture = False}

renderSDL :: StateT SDLContext IO ()
renderSDL = do
    ctx <- get
    SDL.clear (sdlRenderer ctx)
    SDL.copy (sdlRenderer ctx) (cpuTexture ctx) Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 650 0) (SDL.V2 350 800))
    SDL.copy (sdlRenderer ctx) (ppuTexture ctx) Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 000 0) (SDL.V2 650 800))
    SDL.present (sdlRenderer ctx)
    return ()

(<**>) :: (a -> IO a) -> Int -> (a -> IO a)
f <**> 0 = return . id
f <**> 1 = f
f <**> n = (\a -> do
                fa <- f a
                (f <**> (n - 1)) $ fa)

printppu :: PPU.R2C02 -> String
printppu ppu = output where
    cycles = PPU.ppuCycle . PPU.context $ ppu
    scanline = PPU.ppuScanline . PPU.context $ ppu
    complete = PPU.complete . PPU.context $ ppu
    output = "Cycles: " ++ show cycles ++ "\nScanline: " ++ show scanline ++ "\nComplete: " ++ show complete


clearComplete :: NES.NES -> NES.NES
clearComplete n = n{NES.ppu = (NES.ppu n){PPU.context = (PPU.context . NES.ppu $ n){PPU.complete = False}}}


nesLoop :: StateT SDLContext IO ()
nesLoop = do
    ctx <- get
    when (nesRunning ctx) (do
        n' <- liftIO $ ((execStateT NES.tick) <**> 1000) (nes ctx)
        --liftIO . putStrLn $ printppu . NES.ppu $ n'
        let clock = NES.nClock n'
        let updateCPU = MOS.complete . MOS.context . NES.cpu $ n'
        let updatePPU = PPU.complete . PPU.context . NES.ppu $ n'
        put ctx{nes = n', sdlUpdateCPUTexture = updatePPU, sdlUpdatePPUTexture = updatePPU}
        )

mainLoop :: StateT SDLContext IO ()
mainLoop = do
    events <- SDL.pollEvents
    mapM_ handleEvents events
    nesLoop
    updateTextures
    renderSDL
    running <- sdlRunning <$> get
    if running then mainLoop else return ()

main :: IO ()
main = do
    SDL.initializeAll
    Font.initialize
    window <- SDL.createWindow "Shrimp" SDL.defaultWindow{windowInitialSize = SDL.V2 1000 800}
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer{SDL.rendererTargetTexture = True}
    fontA <- Font.load "/home/lesserfish/Documents/Files/Roboto-Light.ttf" 22
    fontB <- Font.load "/home/lesserfish/Documents/Files/Roboto-Light.ttf" 16
    nes <- NES.loadNES "/home/lesserfish/Documents/Code/Shrimp/Tools/Roms/nestest.nes"
    cputext <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget (SDL.V2 350 800)
    pputext <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget (SDL.V2 650 800)

    let context =
            SDLContext
                { sdlWindow = window
                , sdlRenderer = renderer
                , sdlRunning = True
                , sdlFontA = fontA
                , sdlFontB = fontB
                , nes = nes
                , sdlUpdateCPUTexture = True
                , sdlUpdatePPUTexture = True
                , cpuTexture = cputext
                , ppuTexture = pputext
                , nesRunning = False
                }

    execStateT mainLoop context
    SDL.destroyWindow window
    Font.quit
    return ()
