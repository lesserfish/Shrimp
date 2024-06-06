module Frontend.Renderer.Instructions (new, update) where

import Data.Bits
import Data.IORef
import Data.Text (Text, pack)
import Data.Word
import qualified SDL as SDL
import qualified SDL.Font as Font
import Frontend.Common
import Shrimp.NES
import qualified Shrimp.MOS6502 as MOS
import qualified Shrimp.BUS as B
import Text.Printf


renderInstruction :: SDLContext -> Color -> Int -> (Word16, String) -> IO ()
renderInstruction ctx color yoffset (addr, instruction) = do
    let renderData = RenderData (sdlRenderer ctx) (sdlStatusFont ctx)
    let content = "$" ++ toHex4 addr ++ ":    " ++ instruction
    renderString renderData content (1, 25 * yoffset) color

renderInstructions :: SDLContext -> NES -> IO()
renderInstructions ctx nes = do
    let cpu = B.bCPU nes
    let pc = MOS.pc . MOS.registers $ cpu

    instructionsBefore <- MOS.disassembleL (cpu, nes) (pc - 80) (pc + 1) 
    instructionsAfter  <- MOS.disassembleL (cpu, nes) pc (pc + 80) 

    let selectAfter = take 8 instructionsAfter
    let selectBefore = reverse . (drop 1) . (take 7) . reverse $ instructionsBefore

    let selection = selectBefore ++ selectAfter

    let instructionsIDS = [0 .. (length selection - 1)]
    let currentID = length selectBefore
    let colors = [color | id <- instructionsIDS, let color = if id == currentID then cyan else white]

    mapM_ (\id -> renderInstruction ctx (colors !! id) id (selection !! id)) instructionsIDS


update :: SDLContext -> NES -> SDL.Texture -> IO()
update ctx nes texture = do
    let renderer = sdlRenderer ctx
    SDL.rendererRenderTarget renderer SDL.$= Just texture
    SDL.clear renderer
    renderInstructions ctx nes
    SDL.rendererRenderTarget renderer SDL.$= Nothing

new :: SDLContext -> IO SDL.Texture
new ctx = do
    let renderer = sdlRenderer ctx
    let size = SDL.V2 300 350
    SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget size
