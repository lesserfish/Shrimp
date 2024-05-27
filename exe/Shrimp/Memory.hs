module Shrimp.Memory (
    RAM (..),
    new,
    writeByte,
    loadList,
    toList,
    readByte,
    noRAM,
    reset
) where

import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.Vector.Unboxed as UV
import Data.Word

type RAM = UMV.IOVector Word8

new :: Int -> Word8 -> IO RAM
new size e = UMV.replicate (fromIntegral size) e

readByte :: RAM -> Word16 -> IO Word8
readByte ram addr = UMV.read ram (fromIntegral addr)

writeByte :: RAM -> Word16 -> Word8 -> IO ()
writeByte ram addr byte = UMV.write ram (fromIntegral addr) byte

noRAM :: IO RAM
noRAM = UMV.new 0

loadList :: RAM -> Int -> [Word8] -> IO ()
loadList ram offset list = do
    let size = length list 
    let address = [fromIntegral $ offset + x | x <- [0..(size-1)]] :: [Word16]
    let info = zip address list
    mapM_ (\(addr, byte) -> writeByte ram addr byte) info

reset :: RAM -> IO ()
reset ram = do mapM_ (\idx -> UMV.write ram idx 0) [0.. ((fromIntegral . UMV.length $ ram) - 1)]

toList :: RAM -> IO [Word8]
toList ram = do
   fram <- UV.freeze ram 
   return $ UV.toList fram
