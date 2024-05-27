module Renderer.Common where

import Foreign.C.Types
import Communication
import Data.Time.Clock
import qualified SDL as SDL
import qualified SDL.Font as Font
import Data.Text (Text, pack)
import Text.Printf (printf)

data SDLContext = SDLContext
    { cWindow :: SDL.Window
    , cRenderer :: SDL.Renderer
    , cStatusFont :: Font.Font
    }

data RenderData = RenderData
    { rdRenderer :: SDL.Renderer
    , rdFont :: Font.Font
    }

data DisplayMode = DM_CPUSTATUS | DM_NAMETABLE | DM_PATTERNTABLE deriving (Show, Eq)

type NTChoice = Bool
type PTChoice = Bool

nt1 = True
nt2 = False

pt1 = True
pt2 = False

data RenderContext = RenderContext
    { rSDLContext :: SDLContext
    , rtCPUStatus :: SDL.Texture
    , rtCPUInstructions :: SDL.Texture
    , rtNametable :: SDL.Texture
    , rtPattern :: SDL.Texture
    , rtScreen :: SDL.Texture
    , rPipe :: CommPipe
    , rExit :: Bool
    , rRunning :: Bool
    , rUpdateCPU :: Bool
    , rLastTime :: UTCTime
    , rDisplayMode :: DisplayMode
    , rNChoice :: NTChoice
    , rPChoice :: PTChoice
    }

-- Colors
type Color = Font.Color
black :: Color
black = SDL.V4 0 0 0 255
white :: Color
white = SDL.V4 255 255 255 255
red :: Color
red = SDL.V4 255 0 0 255
green :: Color
green = SDL.V4 0 255 0 255
blue :: Color
blue = SDL.V4 0 0 255 255
yellow :: Color
yellow = SDL.V4 255 255 0 255
magenta :: Color
magenta = SDL.V4 255 0 255 255
cyan :: Color
cyan = SDL.V4 0 255 255 255

-- Auxiliary methods


toHex2  :: (Integral a) => a -> String
toHex2 w = printf "%02X" (fromIntegral w :: Int)

toHex4  :: (Integral a) => a -> String
toHex4 w = printf "%04X" (fromIntegral w :: Int)

renderString :: RenderData -> String -> (Int, Int) -> Color -> IO ()
renderString rd string (px, py) color= do
    let text = pack string
    let font = rdFont rd
    let renderer = rdRenderer rd
    (width, height) <- Font.size font text
    let position = SDL.P $ SDL.V2 (fromIntegral px) (fromIntegral py)
    let size = SDL.V2 (fromIntegral width) (fromIntegral height)
    textSurface <- Font.blended font color text
    textTexture <- SDL.createTextureFromSurface renderer textSurface
    SDL.copy renderer textTexture Nothing (Just $ SDL.Rectangle position size)
    SDL.freeSurface textSurface
    SDL.destroyTexture textTexture

windowSegment :: (Int, Int) -> (Int, Int) -> Maybe (SDL.Rectangle CInt)
windowSegment (sx, sy) (w, h) = Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral sx) (fromIntegral sy)) (SDL.V2 (fromIntegral w) (fromIntegral h))

