module Frontend.Simple.Render (render) where

import qualified Shrimp.Utils as B
import Foreign.C.Types (CInt)
import Control.Monad
import Shrimp.NES
import qualified SDL as SDL
import qualified SDL.Font as Font
import Data.Time
import Control.Monad.State
import Frontend.Simple.Common
import qualified Frontend.Simple.Renderer.Display as FRDisplay


frameReady :: StateT RenderContext IO Bool
frameReady = do
    before <- getLastRender
    now <- liftIO $ getCurrentTime
    let diff = diffUTCTime now before
    if diff >= 1/60
        then do
            setLastRender now
            return True 
        else return False

update :: StateT RenderContext IO ()
update = do
    rctx <- get
    ready <- frameReady
    when ready (do
        let screen = rtScreen rctx
        display <- getDisplay
        liftIO $ FRDisplay.update display screen
        )

render :: StateT RenderContext IO ()
render = do
    rctx <- get 
    let renderer = sdlRenderer . rcSDLContext $ rctx
    let screen = rtScreen rctx
    update
    SDL.clear renderer
    SDL.copy renderer screen Nothing Nothing
    SDL.present renderer
