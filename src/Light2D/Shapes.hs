module Light2D.Shapes (
    generateShapesImage, 
    dumpShapesFile
) where

import Codec.Picture.Png
import Codec.Picture.Types

circleSDF :: Float -> Float -> Float -> Float -> Float -> Float
circleSDF x y cx cy r = sqrt (ux * ux + uy * uy) - r where
    ux = x - cx
    uy = y - cy

planeSDF :: Float -> Float -> Float -> Float -> Float -> Float -> Float
planeSDF x y px py nx ny = (x - px) * nx + (y - py) * ny

segmentSDF :: Float -> Float -> Float -> Float -> Float -> Float -> Float
segmentSDF x y ax ay bx by = sqrt (dx * dx + dy * dy) where
    vx = x - ax; vy = y - ay; ux = bx - ax; uy = by - ay
    t = max (min ((vx * ux + vy * uy) / (ux * ux + uy * uy)) 1.0) 0.0
    dx = vx - ux * t; dy = vy - uy * t

capsuleSDF :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
capsuleSDF x y ax ay bx by r = segmentSDF x y ax ay bx by - r

boxSDF :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
boxSDF x y cx cy theta sx sy = min (max dx dy) 0.0 + sqrt (ax * ax + ay * ay) where
    costheta = cos theta; sintheta = sin theta
    dx = abs ((x - cx) * costheta + (y - cy) * sintheta) - sx
    dy = abs ((y - cy) * costheta - (x - cx) * sintheta) - sy
    ax = max dx 0.0; ay = max dy 0.0

triangleSDF :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
triangleSDF x y ax ay bx by cx cy = if 
    (bx - ax) * (y - ay) > (by - ay) * (x - ax) && 
    (cx - bx) * (y - by) > (cy - by) * (x - bx) && 
    (ax - cx) * (y - cy) > (ay - cy) * (x - cx) 
    then -d else d where 
        d = sab `min` sbc `min` sca
        sab = (segmentSDF x y ax ay bx by)
        sbc = (segmentSDF x y bx by cx cy)
        sca = (segmentSDF x y cx cy ax ay)

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
-- scene x y = a .&. b where
--     a = Result (circleSDF x y 0.5 0.5 0.2) 1.0
--     b = Result (planeSDF x y 0.0 0.5 0.0 1.0) 0.8

-- scene x y = Result (capsuleSDF x y 0.4 0.4 0.6 0.6 0.1) 1.0 

-- scene x y = Result (boxSDF x y 0.5 0.5 (2 * pi / 16.0) 0.3 0.1) 1.0

scene x y = Result (triangleSDF x y 0.5 0.2 0.8 0.8 0.3 0.6) 1.0

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


generateShapesImage :: Image PixelRGB8
generateShapesImage = generateImage pixelFunc width height where
    pixelFunc x y = PixelRGB8 v v v where
        fx = fromIntegral x / fromIntegral width
        fy = fromIntegral y / fromIntegral height
        v = floor $ min ((sample  fx fy) * 255.0) 255.0
    height = 512
    width = 512

dumpShapesFile :: IO ()
dumpShapesFile = writePng "img/shapes.png" generateShapesImage
