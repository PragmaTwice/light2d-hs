module Light2D.CSG (
    generateCSGImage, 
    dumpCSGFile
) where

import Codec.Picture.Png
import Codec.Picture.Types

circleSDF :: Float -> Float -> Float -> Float -> Float -> Float
circleSDF x y cx cy r = sqrt (ux * ux + uy * uy) - r where
    ux = x - cx
    uy = y - cy

data Result = Result {
    sd :: Float,
    emissive :: Float
}

(.|.) :: Result -> Result -> Result
a .|. b = if sd a < sd b then a else b

(.&.) :: Result -> Result -> Result
a .&. b = if sd a > sd b then Result (sd a) (emissive b) else Result (sd b) (emissive a)

(.-.) :: Result -> Result -> Result
a .-. b = Result (if sd a > -sd b then sd a else -sd b) (emissive a)

scene :: Float -> Float -> Result
scene x y = r1  .|. r2 .|. r3 .-. r4 where
    r1 = Result (circleSDF x y 0.3 0.3 0.10) 2.0
    r2 = Result (circleSDF x y 0.3 0.7 0.05) 0.8
    r3 = Result (circleSDF x y 0.7 0.5 0.10) 0.0
    r4 = Result (circleSDF x y 0.4 0.3 0.10) 1.8

trace :: Float -> Float -> Float -> Float -> Float
trace ox oy dx dy = traceImpl 0 0 where
    traceImpl i t = if i < maxStep && t < maxDistance
        then let r = scene (ox + dx * t) (oy + dy * t) in
            if sd r < epsilon then emissive r
            else traceImpl (i + 1) (t + sd r)
        else 0
    maxStep = 10
    maxDistance = 2.0
    epsilon = 1e-6

sample :: Float -> Float -> Float
sample x y = (foldl (+) 0 $ map traceFunc [0..n - 1]) / n where
    traceFunc i = trace x y (cos $ a i) (sin $ a i)
    a i = 2 * pi * i / n
    n = 64 * 4


generateCSGImage :: Image PixelRGB8
generateCSGImage = generateImage pixelFunc width height where
    pixelFunc x y = PixelRGB8 v v v where
        fx = fromIntegral x / fromIntegral width
        fy = fromIntegral y / fromIntegral height
        v = floor $ min ((sample  fx fy) * 255.0) 255.0
    height = 512
    width = 512

dumpCSGFile :: IO ()
dumpCSGFile = writePng "img/csg.png" generateCSGImage
