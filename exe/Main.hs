{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Identity
import Control.Monad.State
import Data.Bits
import Data.Text (Text, pack)
import Data.Word
import SDL (WindowConfig (windowInitialSize))
import qualified SDL as SDL
import qualified SDL.Font as Font
import qualified Shrimp.MOS6502 as MOS
import qualified Shrimp.NES as NES
import Text.Printf

data SDLContext = SDLContext
    { sdlWindow :: SDL.Window
    , sdlRenderer :: SDL.Renderer
    , sdlRunning :: Bool
    , sdlFontA :: Font.Font
    , sdlFontB :: Font.Font
    , nes :: NES.NES
    , sdlUpdateTextures :: Bool
    , cpuTexture :: SDL.Texture
    , nesRunning :: Bool
    }

toHex :: Word8 -> String
toHex w = printf "%02X" w

toHex' :: Word16 -> String
toHex' w = printf "%04X" w

til :: NES.NES -> NES.NES
til n = output
  where
    cycles_left = MOS.cycles . NES.cpu $ n
    cc = mod (NES.nClock $ n) 3
    output = if cycles_left == 0 && cc == 0 then n else til $ execState NES.tick n

handleKeydown :: SDL.Keycode -> StateT SDLContext IO ()
handleKeydown SDL.KeycodeQ = modify (\ctx -> ctx{sdlRunning = False})
handleKeydown SDL.KeycodeSpace = modify (\ctx -> ctx{nesRunning = not (nesRunning ctx)})
handleKeydown SDL.KeycodeN = do
    ctx <- get
    let n' = execState NES.tick (nes ctx)
    put ctx{nes = n', sdlUpdateTextures = True}
handleKeydown SDL.KeycodeC = do
    ctx <- get
    let n' = execState NES.tick (til . nes $ ctx)
    put ctx{nes = n', sdlUpdateTextures = True}
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
    let ps = MOS.ps . MOS.mosRegisters . NES.cpu $ n :: Word8
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
    let pc = toHex' . MOS.pc . MOS.mosRegisters . NES.cpu $ n
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
    let reg = toHex . MOS.acc . MOS.mosRegisters . NES.cpu $ n
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
    let reg = toHex . MOS.idx . MOS.mosRegisters . NES.cpu $ n
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
    let reg = toHex . MOS.idy . MOS.mosRegisters . NES.cpu $ n
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
    let reg = toHex' . fromIntegral . MOS.sp . MOS.mosRegisters . NES.cpu $ n
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
    let txt = pack $ "$" ++ toHex' addr ++ ":   " ++ (if inst == "" then replicate 48 ' ' else inst)
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

updateCPUInstruction :: StateT SDLContext IO ()
updateCPUInstruction = do
    ctx <- get
    let n = nes ctx
    let pc = MOS.pc . MOS.mosRegisters . NES.cpu $ n
    let after = take 8 . runIdentity . (MOS.disassembleL pc (pc + 20 * 0x04)) $ n
    let before = tail . take 8 . reverse . runIdentity . (MOS.disassembleL (pc - 20 * 0x04) pc) $ n
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

updateTextures :: StateT SDLContext IO ()
updateTextures = do
    ctx <- get
    if sdlUpdateTextures ctx
        then do
            updateCPUTexture
            put ctx{sdlUpdateTextures = False}
        else return ()

renderSDL :: StateT SDLContext IO ()
renderSDL = do
    ctx <- get
    SDL.clear (sdlRenderer ctx)
    SDL.copy (sdlRenderer ctx) (cpuTexture ctx) Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 500 0) (SDL.V2 700 1600))
    SDL.present (sdlRenderer ctx)
    return ()

(<**>) :: (a -> a) -> Int -> (a -> a)
f <**> 0 = id
f <**> 1 = f
f <**> n = f . (f <**> (n - 1))

nesLoop :: StateT SDLContext IO ()
nesLoop = do
    ctx <- get
    if nesRunning ctx
        then do
            let n = nes ctx
            let n' = (execState NES.tick <**> 10000) n
            put ctx{nes = n', sdlUpdateTextures = True}
        else return ()
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
    window <- SDL.createWindow "Shrimp" SDL.defaultWindow{windowInitialSize = SDL.V2 800 800}
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer{SDL.rendererTargetTexture = True}
    fontA <- Font.load "/home/lesserfish/Documents/Files/Roboto-Light.ttf" 22
    fontB <- Font.load "/home/lesserfish/Documents/Files/Roboto-Light.ttf" 16
    nes <- NES.loadNES "/home/lesserfish/Documents/Code/Shrimp/Tools/Roms/nestest.nes"
    cputext <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget (SDL.V2 700 1600)

    let context =
            SDLContext
                { sdlWindow = window
                , sdlRenderer = renderer
                , sdlRunning = True
                , sdlFontA = fontA
                , sdlFontB = fontB
                , nes = nes
                , sdlUpdateTextures = True
                , cpuTexture = cputext
                , nesRunning = False
                }

    execStateT mainLoop context
    SDL.destroyWindow window
    Font.quit
    return ()
