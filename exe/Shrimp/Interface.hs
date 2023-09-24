module Shrimp.Interface where

import Data.Word

data ControllerID = Controller1 | Controller2
class AbstractInterface a where
    getInput :: a -> ControllerID -> Word8 -- Gets the byte associated to the current controller i state
    setPixel :: a -> (Word8, Word8) -> Int -> a
