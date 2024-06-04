module Shrimp.Display (
    Display (..),
    newDisplay,
    setPixel,
    getPixel,
    toList,
    toByteString,
    toByteString',
    reset
) where

import Data.Word
import qualified Data.ByteString as BS
import qualified Shrimp.Memory as Memory

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

data LineBuffer = LineBuffer
    { lVBuffer :: Memory.RAM
    , lPBuffer :: Memory.RAM
    }

newLineBuffer :: IO LineBuffer
newLineBuffer = do
    vb <- Memory.new 256 0x3F
    pb <- Memory.new 256 0x01
    return $ LineBuffer vb pb

setSPixel :: LineBuffer -> Word16 -> (Word8, Word8) -> IO ()
setSPixel lb x (color, priority) = do
    Memory.writeByte (lVBuffer lb) x color
    Memory.writeByte (lPBuffer lb) x priority

getSPixel :: LineBuffer -> Word16 -> IO (Word8, Word8)
getSPixel lb x = do
    color <- Memory.readByte (lVBuffer lb) x
    priority <- Memory.readByte (lPBuffer lb) x
    return $ (color, priority)

resetLB :: LineBuffer -> IO ()
resetLB lb = do
    Memory.reset' (lVBuffer lb) 0x3F
    Memory.reset' (lPBuffer lb) 0x01
