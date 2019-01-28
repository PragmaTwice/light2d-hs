module Light2D.Reflection (
    generateReflectionImage, 
    dumpReflectionFile
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
    emissive :: Float,
    reflectivity :: Float
}

(.|.) :: Result -> Result -> Result
a .|. b = if sd a < sd b then a else b

(.&.) :: Result -> Result -> Result
a .&. b = if sd a > sd b then a else b

(.-.) :: Result -> Result -> Result
a .-. b = Result (if sd a > -sd b then sd a else -sd b) (emissive a) (reflectivity a)

epsilon :: Float
epsilon = 1e-6

reflect :: Float -> Float -> Float -> Float -> (Float, Float)
reflect ix iy nx ny = (ix - idotn2 * nx, iy - idotn2 * ny) where
    idotn2 = (ix * nx + iy * ny) * 2.0

gradient :: Float -> Float -> (Float, Float)
gradient x y = (
    (sd (scene (x + epsilon) y) - sd (scene (x - epsilon) y)) * (0.5 / epsilon), 
    (sd (scene x (y + epsilon)) - sd (scene x (y - epsilon))) * (0.5 / epsilon))

scene :: Float -> Float -> Result
-- scene x y = a .|. b .|. c where
--     a = Result (circleSDF x y 0.4 0.2 0.1) 2.0 0.0
--     b = Result (boxSDF x y 0.5 0.8 (2 * pi / 16.0) 0.1 0.1) 0.0 0.9
--     c = Result (boxSDF x y 0.8 0.5 (2 * pi / 16.0) 0.1 0.1) 0.0 0.9

scene x y = a .|. (b .-. c) where
    a = Result (circleSDF x y 0.4 0.2 0.1) 2.0 0.0
    b = Result (planeSDF x y 0.0 0.5 0.0 (-1.0)) 0.0 0.9
    c = Result (circleSDF x y 0.5 0.5 0.4) 0.0 0.9

trace :: Float -> Float -> Float -> Float -> Int -> Float
trace ox oy dx dy depth = traceImpl 0 0 where
    traceImpl i t = if i < maxStep && t < maxDistance
        then let 
            x = ox + dx * t
            y = oy + dy * t
            r = scene x y in
            if sd r < epsilon then let
                sum = emissive r in
                if depth < maxDepth && reflectivity r > 0.0
                    then let
                        (nx, ny) = gradient x y
                        (rx, ry) = reflect dx dy nx ny in 
                            sum + reflectivity r * (trace (x + nx * bias) (y + ny * bias) rx ry (depth + 1))
                    else sum
            else traceImpl (i + 1) (t + sd r)
        else 0
    maxStep = 64
    maxDistance = 5.0
    maxDepth = 3
    bias = 1e-4

sample :: Float -> Float -> Float
sample x y = (foldl (+) 0 $ map traceFunc [0..n - 1]) / n where
    traceFunc i = trace x y (cos $ a i) (sin $ a i) 0
    a i = 2 * pi * i / n
    n = 64 * 4


generateReflectionImage :: Image PixelRGB8
generateReflectionImage = generateImage pixelFunc width height where
    pixelFunc x y = PixelRGB8 v v v where
        fx = fromIntegral x / fromIntegral width
        fy = fromIntegral y / fromIntegral height
        v = floor $ min ((sample  fx fy) * 255.0) 255.0
    height = 512
    width = 512

dumpReflectionFile :: IO ()
dumpReflectionFile = writePng "img/reflection.png" generateReflectionImage
