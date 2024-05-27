module Shrimp.Display where

import Data.Word
import qualified Data.ByteString as BS
import qualified Shrimp.Memory as M

data Display = Display
    { dVideoBuffer :: M.RAM
    }

newDisplay :: IO Display
newDisplay = do
    buffer <- M.new (256 * 240) 0
    return $ Display buffer


setPixel :: Display -> (Word16, Word16) -> Word8 -> IO ()
setPixel d (x, y) px = M.writeByte (dVideoBuffer d) (y * 256 + x) px

getPixel :: Display -> (Word16, Word16) -> IO Word8
getPixel d (x, y) = M.readByte (dVideoBuffer d) (y * 256 + x)

toList :: Display -> IO [Word8]
toList = M.toList . dVideoBuffer
    
toByteString :: Display -> (Word8 -> BS.ByteString) -> IO BS.ByteString
toByteString d colorMap = (BS.concat . (fmap colorMap)) <$> (toList d)

toByteString' :: Display -> (Word8 -> [BS.ByteString]) -> IO BS.ByteString
toByteString' d colorMap = (BS.concat . concat . (fmap colorMap)) <$> (toList d)

reset :: Display -> IO ()
reset d = M.reset (dVideoBuffer d)
