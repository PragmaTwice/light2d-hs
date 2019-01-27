module Light2D.Basic where

import Codec.Picture.Png
import Codec.Picture.Types

circleSDF :: Float -> Float -> Float -> Float -> Float -> Float
circleSDF x y cx cy r = sqrt (ux * ux + uy * uy) - r where
    ux = x - cx
    uy = y - cy

trace :: Float -> Float -> Float -> Float -> Float
trace ox oy dx dy = traceImpl 0 0 where
    traceImpl i t = if i < maxStep && t < maxDistance
        then let sd = circleSDF (ox + dx * t) (oy + dy * t) 0.5 0.5 0.1 in
            if sd < epsilon then 2
            else traceImpl (i + 1) (t + sd)
        else 0
    maxStep = 10
    maxDistance = 2.0
    epsilon = 1e-6

sample :: Float -> Float -> Float
sample x y = (foldl (+) 0 $ map traceFunc [0..n - 1]) / n where
    traceFunc i = trace x y (cos $ a i) (sin $ a i)
    a i = 2 * pi * i / n
    n = 64


generateBasicImage :: Image PixelRGB8
generateBasicImage = generateImage pixelFunc width height where
    pixelFunc x y = PixelRGB8 v v v where
        fx = fromIntegral x / fromIntegral width
        fy = fromIntegral y / fromIntegral height
        v = floor $ min ((sample  fx fy) * 255.0) 255.0
    height = 512
    width = 512

dumpBasicFile :: IO ()
dumpBasicFile = writePng "img/basic.png" generateBasicImage
