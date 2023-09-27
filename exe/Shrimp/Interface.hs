{-# LANGUAGE GADTs #-}

module Shrimp.Interface where

import Data.Word

data ControllerID = Controller1 | Controller2
class AbstractInterface a where
    getInput :: a -> ControllerID -> Word8 -- Gets the byte associated to the current controller i state
    setPixel :: a -> (Word8, Word8) -> Int -> a

data Interface = forall a. (AbstractInterface a) => Interface {interface :: a}

instance AbstractInterface Interface where
    getInput (Interface i) cid = getInput i cid
    setPixel (Interface i) position value = Interface (setPixel i position value)
