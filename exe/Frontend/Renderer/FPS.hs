module Frontend.Renderer.FPS (new, update) where

import Data.List
import Frontend.Common
import Shrimp.NES
import Data.Word
import qualified SDL as SDL
import qualified Shrimp.BUS as B
import qualified Shrimp.Memory as Memory
import qualified Shrimp.Cartridge as Cartridge


renderFPS :: SDLContext -> Int -> SDL.Texture -> IO ()
renderFPS ctx fps texture = do
    let renderData = RenderData (sdlRenderer ctx) (sdlStatusFont ctx)
    let content = show fps
    renderString renderData content (0, 0) white

update :: SDLContext -> Int -> SDL.Texture -> IO ()
update ctx fps texture = do
    let renderer = sdlRenderer ctx
    SDL.rendererRenderTarget renderer SDL.$= Just texture
    SDL.clear renderer
    renderFPS ctx fps texture
    SDL.rendererRenderTarget renderer SDL.$= Nothing

new :: SDLContext -> IO SDL.Texture
new ctx = do
    let renderer = sdlRenderer ctx
    let size = SDL.V2 20 20
    SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget size
