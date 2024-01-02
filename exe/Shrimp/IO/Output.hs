module Shrimp.IO.Output where

data Position = Position {pX :: Int, pY :: Int}
data Color = Color {cRed :: Int, cGreen :: Int, cBlue :: Int}
data Pixel = Pixel {pPosition :: Position, pColor :: Color}
data Request = PUT Pixel
