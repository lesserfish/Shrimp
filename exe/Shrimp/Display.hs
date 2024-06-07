module Shrimp.Display (
    Display (..),
    newDisplay,
    setPixel,
    getPixel,
    toList,
    toByteString,
    toByteString',
    reset,
    LineBuffer(..),
    Priority(..),
    setLBPixel,
    trySetLBPixel,
    getLBPixel,
    resetLB,
    newLineBuffer
) where

import Control.Monad (when)
import Data.Word
import qualified Data.ByteString as BS
import qualified Shrimp.Memory as Memory
import GHC.Arr (writeSTArray)
import Shrimp.MOS6502 (setSP)

data Display = Display
    { dBuffer :: Memory.RAM
    }

newDisplay :: IO Display
newDisplay = do
    buffer <- Memory.new (256 * 240) 0x3F
    return $ Display buffer


setPixel :: Display -> (Word16, Word16) -> Word8 -> IO ()
setPixel d (x, y) px = Memory.writeByte (dBuffer d) (y * 256 + x) px

getPixel :: Display -> (Word16, Word16) -> IO Word8
getPixel d (x, y) = Memory.readByte (dBuffer d) (y * 256 + x)

toList :: Display -> IO [Word8]
toList = Memory.toList . dBuffer
    
toByteString :: Display -> (Word8 -> BS.ByteString) -> IO BS.ByteString
toByteString d colorMap = (BS.concat . (fmap colorMap)) <$> (toList d)

toByteString' :: Display -> (Word8 -> [BS.ByteString]) -> IO BS.ByteString
toByteString' d colorMap = (BS.concat . concat . (fmap colorMap)) <$> (toList d)

reset :: Display -> IO ()
reset d = Memory.reset' (dBuffer d) 0x3F


data Priority = FRONT | BACK | UNSET deriving (Show, Eq)

p2w :: Priority -> Word8
p2w FRONT = 0
p2w BACK = 1
p2w UNSET = 2

w2p :: Word8 -> Priority
w2p 0 = FRONT 
w2p 1 = BACK 
w2p _ = UNSET

data LineBuffer = LineBuffer
    { lPixelBuffer :: Memory.RAM
    , lPaletteBuffer :: Memory.RAM
    , lPriorityBuffer :: Memory.RAM
    , lWidth :: Int
    }

newLineBuffer :: Int -> IO LineBuffer
newLineBuffer width = do
    pib <- Memory.new width 0x00
    pab <- Memory.new width 0x00
    prb <- Memory.new width 0x01
    return $ LineBuffer pib pab prb width

setLBPixel :: LineBuffer -> Int -> (Word8, Word8, Priority) -> IO ()
setLBPixel lb x' (pixel, palette, priority)
    | x' >= (lWidth lb) = return ()
    | otherwise = do
        let x = fromIntegral x'
        Memory.writeByte (lPixelBuffer lb) x pixel
        Memory.writeByte (lPaletteBuffer lb) x palette
        Memory.writeByte (lPriorityBuffer lb) x (p2w priority)

trySetLBPixel :: LineBuffer -> Int -> (Word8, Word8, Priority) -> IO ()
trySetLBPixel lb x' info
    | x' >= (lWidth lb) = return ()
    | otherwise = do
        let x = fromIntegral x'
        currentPriority <- w2p <$> Memory.readByte (lPriorityBuffer lb) x
        when (currentPriority /= FRONT) (setLBPixel lb x' info)

getLBPixel :: LineBuffer -> Int -> IO (Word8, Word8, Priority)
getLBPixel lb x'
    | x' >= (lWidth lb) = return (0, 0, UNSET)
    | otherwise = do
        let x = fromIntegral x'
        pixel <- Memory.readByte (lPixelBuffer lb) x
        palette <- Memory.readByte (lPaletteBuffer lb) x
        priority <- w2p <$> Memory.readByte (lPriorityBuffer lb) x
        return $ (pixel, palette, priority)

resetLB :: LineBuffer -> IO ()
resetLB lb = do
    Memory.reset' (lPixelBuffer lb) 0x00
    Memory.reset' (lPaletteBuffer lb) 0x00
    Memory.reset' (lPriorityBuffer lb) 0x01
