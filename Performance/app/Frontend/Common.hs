module Frontend.Common where


import Shrimp.NES
import Control.Monad.State
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Data.Word
import qualified Data.ByteString as BS
import Foreign.C.Types
import Communication
import Data.Time.Clock
import qualified SDL as SDL
import qualified SDL.Font as Font
import Data.Text (Text, pack)
import Text.Printf (printf)

data SDLContext = SDLContext
    { sdlWindow :: SDL.Window
    , sdlRenderer :: SDL.Renderer
    , sdlStatusFont :: Font.Font
    , sdlNametableFont :: Font.Font
    }

data RenderData = RenderData
    { rdRenderer :: SDL.Renderer
    , rdFont :: Font.Font
    }

data DisplayMode = DM_SCREEN 
                 | DM_NAMETABLE 
                 | DM_INSTRUCTION 
                 | DM_PATTERN_1 
                 | DM_PATTERN_2 
                 deriving (Show, Eq)

data RenderStatus = RenderStatus
    { rsExit :: Bool
    , rsLastRender :: UTCTime
    , rsUpdateTextures :: Bool
    , rsRunning :: Bool
    }

data RenderTextures = RenderTextures
    { rtCPUStatus :: SDL.Texture
    , rtCPUInstructions :: SDL.Texture
    , rtPattern :: SDL.Texture
    , rtPalette :: SDL.Texture
    , rtDisplay :: SDL.Texture
    }

data RenderContext = RenderContext
    { rcTextures :: RenderTextures
    , rcStatus :: RenderStatus
    , rcCommunicationPipe :: CommPipe
    , rcLDisplayMode :: DisplayMode
    , rcRDisplayMode :: DisplayMode
    , rcSDLContext :: SDLContext
    }


-- Getters / Setters

getRTE :: StateT RenderContext IO (TChan Command)
getRTE = rte . rcCommunicationPipe <$> get

getETR :: StateT RenderContext IO (TChan Feedback)
getETR = etr . rcCommunicationPipe <$> get

getTNES :: StateT RenderContext IO (TVar NES)
getTNES = tNES . rcCommunicationPipe <$> get

setExit :: Bool -> StateT RenderContext IO ()
setExit v = modify (\rctx -> rctx {rcStatus = (rcStatus rctx){rsExit = v}})

getExit :: StateT RenderContext IO Bool
getExit = rsExit . rcStatus <$> get

getNES :: StateT RenderContext IO NES
getNES = do
    tnes <- getTNES
    nes <- liftIO $ atomically . readTVar $ tnes
    return nes

setRunning :: Bool -> StateT RenderContext IO ()
setRunning v = modify (\rctx -> rctx {rcStatus = (rcStatus rctx){rsRunning = v}})

getRunning :: StateT RenderContext IO Bool
getRunning = rsRunning . rcStatus <$> get


setUpdateTextures :: Bool -> StateT RenderContext IO ()
setUpdateTextures v = modify (\rctx -> rctx {rcStatus = (rcStatus rctx){rsUpdateTextures = v}})

getUpdateTextures :: StateT RenderContext IO Bool
getUpdateTextures = rsUpdateTextures . rcStatus <$> get

getSDLContext :: StateT RenderContext IO SDLContext
getSDLContext = rcSDLContext <$> get

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


-- Colors


type Color = Font.Color
black :: Color
black = SDL.V4 0 0 0 255
gray :: Color
gray = SDL.V4 180 180 180 255
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



toColor :: Word8 -> BS.ByteString
toColor 0x00 = BS.pack $ [255, 84, 84, 84]
toColor 0x01 = BS.pack $ [255, 116, 30, 0]
toColor 0x02 = BS.pack $ [255, 144, 16, 8]
toColor 0x03 = BS.pack $ [255, 136, 0, 48]
toColor 0x04 = BS.pack $ [255, 100, 0, 68]
toColor 0x05 = BS.pack $ [255, 48, 0, 92]
toColor 0x06 = BS.pack $ [255, 0, 4, 84]
toColor 0x07 = BS.pack $ [255, 0, 24, 60]
toColor 0x08 = BS.pack $ [255, 0, 42, 32]
toColor 0x09 = BS.pack $ [255, 0, 58, 8]
toColor 0x0A = BS.pack $ [255, 0, 64, 0]
toColor 0x0B = BS.pack $ [255, 0, 60, 0]
toColor 0x0C = BS.pack $ [255, 60, 50, 0]
toColor 0x0D = BS.pack $ [255, 0, 0, 0]
toColor 0x0E = BS.pack $ [255, 0, 0, 0]
toColor 0x0F = BS.pack $ [255, 0, 0, 0]
toColor 0x10 = BS.pack $ [255, 152, 150, 152]
toColor 0x11 = BS.pack $ [255, 196, 76, 8]
toColor 0x12 = BS.pack $ [255, 236, 50, 48]
toColor 0x13 = BS.pack $ [255, 228, 30, 92]
toColor 0x14 = BS.pack $ [255, 176, 20, 136]
toColor 0x15 = BS.pack $ [255, 100, 20, 160]
toColor 0x16 = BS.pack $ [255, 32, 34, 152]
toColor 0x17 = BS.pack $ [255, 0, 60, 120]
toColor 0x18 = BS.pack $ [255, 0, 90, 84]
toColor 0x19 = BS.pack $ [255, 0, 114, 40]
toColor 0x1A = BS.pack $ [255, 0, 124, 8]
toColor 0x1B = BS.pack $ [255, 40, 118, 0]
toColor 0x1C = BS.pack $ [255, 120, 102, 0]
toColor 0x1D = BS.pack $ [255, 0, 0, 0]
toColor 0x1E = BS.pack $ [255, 0, 0, 0]
toColor 0x1F = BS.pack $ [255, 0, 0, 0]
toColor 0x20 = BS.pack $ [255, 236, 238, 236]
toColor 0x21 = BS.pack $ [255, 236, 154, 76]
toColor 0x22 = BS.pack $ [255, 236, 124, 120]
toColor 0x23 = BS.pack $ [255, 236, 98, 176]
toColor 0x24 = BS.pack $ [255, 236, 84, 228]
toColor 0x25 = BS.pack $ [255, 180, 88, 236]
toColor 0x26 = BS.pack $ [255, 100, 106, 236]
toColor 0x27 = BS.pack $ [255, 32, 136, 212]
toColor 0x28 = BS.pack $ [255, 0, 170, 160]
toColor 0x29 = BS.pack $ [255, 0, 196, 116]
toColor 0x2A = BS.pack $ [255, 32, 208, 76]
toColor 0x2B = BS.pack $ [255, 108, 204, 56]
toColor 0x2C = BS.pack $ [255, 204, 180, 56]
toColor 0x2D = BS.pack $ [255, 60, 60, 60]
toColor 0x2E = BS.pack $ [255, 0, 0, 0]
toColor 0x2F = BS.pack $ [255, 0, 0, 0]
toColor 0x30 = BS.pack $ [255, 236, 238, 236]
toColor 0x31 = BS.pack $ [255, 236, 204, 168]
toColor 0x32 = BS.pack $ [255, 236, 188, 188]
toColor 0x33 = BS.pack $ [255, 236, 178, 212]
toColor 0x34 = BS.pack $ [255, 236, 174, 236]
toColor 0x35 = BS.pack $ [255, 212, 174, 236]
toColor 0x36 = BS.pack $ [255, 176, 180, 236]
toColor 0x37 = BS.pack $ [255, 144, 196, 228]
toColor 0x38 = BS.pack $ [255, 120, 210, 204]
toColor 0x39 = BS.pack $ [255, 120, 222, 180]
toColor 0x3A = BS.pack $ [255, 144, 226, 168]
toColor 0x3B = BS.pack $ [255, 180, 226, 152]
toColor 0x3C = BS.pack $ [255, 228, 214, 160]
toColor 0x3D = BS.pack $ [255, 160, 162, 160]
toColor 0x3E = BS.pack $ [255, 0, 0, 0]
toColor 0x3F = BS.pack $ [255, 0, 0, 0]
toColor _ = BS.pack [0x00, 0x00, 0x00, 0x00]
