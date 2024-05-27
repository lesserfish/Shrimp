module Shrimp.Memory (
    RAM (..),
    IORAM,
    STRAM,
    new,
    fromList,
    fromListIO,
    newIO,
    newST,
    writeByte,
    writeByteIO,
    PrimMonad,
    PrimState,
    loadList,
    readByte,
    readByteIO,
    noRAM,
    RealWorld,
    reset,
    resetIO
) where

import Data.Vector.Unboxed.Mutable (PrimMonad, PrimState, RealWorld)
import qualified Data.Vector.Unboxed.Mutable as UMV
import Control.Monad.ST
import Data.Word

type RAM s = UMV.MVector s Word8
type IORAM = UMV.IOVector Word8
type STRAM = UMV.STVector Word8

new :: (PrimMonad m) => Int -> Word8 -> m (RAM (PrimState m))
new size e = UMV.replicate (fromIntegral size) e

newIO :: Int -> Word8 -> IO IORAM
newIO = new

newST :: Int -> Word8 -> ST s (RAM (PrimState (ST s)))
newST = new

readByte :: (PrimMonad m) => RAM (PrimState m) -> Word16 -> m Word8
readByte ram addr = UMV.read ram (fromIntegral addr)

readByteIO :: IORAM -> Word16 -> IO Word8
readByteIO = readByte

writeByte :: (PrimMonad m) => RAM (PrimState m) -> Word16 -> Word8 -> m ()
writeByte ram addr byte = UMV.write ram (fromIntegral addr) byte

writeByteIO :: IORAM -> Word16 -> Word8 -> IO ()
writeByteIO = writeByte

noRAM :: (PrimMonad m) => m (RAM (PrimState m))
noRAM = UMV.new 0

loadList :: (PrimMonad m)  => RAM (PrimState m) -> Int -> [Word8] -> m ()
loadList ram offset lst
    | UMV.length ram < length lst = error "Memory too small to load entire list"
    | otherwise = mapM_ (\idx -> UMV.write ram (offset + idx) (lst !! idx)) [0.. ((fromIntegral . length $ lst) - 1)]

fromList :: (PrimMonad m) => [Word8] -> m (RAM (PrimState m))
fromList list = UMV.generate (length list) (\idx -> list !! idx)

fromListIO :: [Word8] -> IO IORAM
fromListIO = fromList

reset :: (PrimMonad m) => RAM (PrimState m) ->  m ()
reset ram = do mapM_ (\idx -> UMV.write ram idx 0) [0.. ((fromIntegral . UMV.length $ ram) - 1)]

resetIO :: IORAM -> IO ()
resetIO = reset

