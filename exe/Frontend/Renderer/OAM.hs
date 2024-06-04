module Frontend.Renderer.OAM (new, update) where

import Data.List
import Frontend.Common
import Shrimp.NES
import Data.Word
import qualified SDL as SDL
import Control.Monad.State
import qualified Shrimp.BUS as B
import qualified Shrimp.R2C02 as R2C02
import qualified Shrimp.Memory as Memory
import qualified Shrimp.Cartridge as Cartridge
import Data.IORef

renderOAM :: SDLContext -> NES -> IO()
renderOAM ctx nes = do
        let renderData = RenderData (sdlRenderer ctx) (sdlStatusFont ctx)
        let ppu = B.bPPU nes
        mapM_ (\id -> do
            (sprite, _) <- runStateT (R2C02.getSprite id) ppu
            let content =  "Y: " ++ (toHex2 . R2C02.sprY $ sprite)
                        ++ "   X: " ++ (toHex2 . R2C02.sprX $ sprite)
                        ++ "   ID: " ++ (toHex2. R2C02.sprTile $ sprite)
                        ++ "   Att: " ++ (toHex2 . R2C02.sprAttr $ sprite)

            let yoffset = 25 * fromIntegral id
            renderString renderData content (1, yoffset) white
            ) [0..9]
        return ()

update :: SDLContext -> NES -> SDL.Texture -> IO ()
update ctx nes texture = do
    let renderer = sdlRenderer ctx
    SDL.rendererRenderTarget renderer SDL.$= Just texture
    SDL.clear renderer
    renderOAM ctx nes
    SDL.rendererRenderTarget renderer SDL.$= Nothing

new :: SDLContext -> IO SDL.Texture
new ctx = do
    let renderer = sdlRenderer ctx
    let size = SDL.V2 300 350
    SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget size
