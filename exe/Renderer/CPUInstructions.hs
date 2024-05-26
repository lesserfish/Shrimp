module Renderer.CPUInstructions where

import Control.Monad.State
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits
import Data.Text (Text, pack)
import Data.Word
import qualified SDL as SDL
import qualified SDL.Font as Font
import Renderer.Common
import Shrimp.NES
import qualified Shrimp.MOS6502 as MOS
import Text.Printf

execute :: (Monad m) => (() -> m b) -> m b
execute f = f ()

disassembleL' :: Word16 -> Word16 -> StateT NES IO [(Word16, String)]
disassembleL' start end = execute $ (MOS.disassembleL start end)

disassembleL :: Word16 -> Word16 -> NES -> IO [(Word16, String)]
disassembleL start end nes = do
    (output, _) <- runStateT (disassembleL' start end) nes
    return output

getTextHeight :: SDLContext -> IO (Int, Int)
getTextHeight ctx = do
    let font = cStatusFont ctx
    Font.size font (pack "M")

renderCPUInstruction :: SDLContext -> Color -> Int -> (Word16, String) -> IO ()
renderCPUInstruction ctx color yoffset (addr, instruction) = do
    let renderData = RenderData (cRenderer ctx) (cStatusFont ctx)
    let content = "$" ++ toHex4 addr ++ ":    " ++ instruction
    renderString renderData content (1, 25 * yoffset) color

renderCPUInstructions :: SDLContext -> NES -> IO()
renderCPUInstructions ctx nes = do
    let pc = MOS.pc . MOS.registers . cpu $ nes
    instructionsBefore <- disassembleL (pc - 80) pc nes
    instructionsAfter  <- disassembleL pc (pc + 80) nes

    let selectAfter = take 8 instructionsAfter
    let selectBefore = reverse . (drop 1) . (take 7) . reverse $ instructionsBefore

    let selection = selectBefore ++ selectAfter

    let instructionsIDS = [0 .. (length selection - 1)]
    let currentID = length selectBefore
    let colors = [color | id <- instructionsIDS, let color = if id == currentID then cyan else white]

    mapM_ (\id -> renderCPUInstruction ctx (colors !! id) id (selection !! id)) instructionsIDS


updateInstructionTexture :: (MonadIO m) => SDLContext -> NES -> SDL.Texture -> m()
updateInstructionTexture ctx nes texture = do
    let renderer = cRenderer ctx
    SDL.rendererRenderTarget renderer SDL.$= Just texture
    SDL.clear renderer
    liftIO $ renderCPUInstructions ctx nes
    SDL.rendererRenderTarget renderer SDL.$= Nothing

createInstructionTexture :: SDLContext -> IO SDL.Texture
createInstructionTexture ctx = do
    let renderer = cRenderer ctx
    let size = SDL.V2 300 350
    SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget size
