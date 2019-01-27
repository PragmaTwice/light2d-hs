module Lib
    ( dumpRedPng
    ) where

import Codec.Picture.Png
import Codec.Picture.Types

dumpRedPng :: IO ()
dumpRedPng = createMutableImage 512 512 (PixelRGB8 255 0 0) 
    >>= freezeImage
    >>= writePng "temp.png"
