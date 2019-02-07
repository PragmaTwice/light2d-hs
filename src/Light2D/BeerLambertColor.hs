module Light2D.BeerLambertColor (
    generateBeerLambertColorImage, 
    dumpBeerLambertColorFile
) where

import Codec.Picture.Png
import Codec.Picture.Types
import Data.Fixed

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

ngonSDF :: Float -> Float -> Float -> Float -> Float -> Float -> Float
ngonSDF x y cx cy r n = planeSDF (s * cos t) (s * sin t) r 0.0 (cos (a * 0.5)) (sin (a * 0.5)) where
    ux = x - cx; uy = y - cy; a = 2 * pi / n
    t = ((atan2 uy ux) + 2 * pi) `mod'` a; s = sqrt (ux * ux + uy * uy)

data Color = Color {
    red :: Float,
    green :: Float,
    blue :: Float
}

black :: Color
black = Color 0 0 0

(#+#) :: Color -> Color -> Color
a #+# b = Color (red a + red b) (green a + green b) (blue a + blue b)

(#*#) :: Color -> Color -> Color
a #*# b = Color (red a * red b) (green a * green b) (blue a * blue b)

(#*) :: Color -> Float -> Color
a #* b = Color (red a * b) (green a * b) (blue a * b)

(*#) :: Float -> Color -> Color
a *# b = Color (a * red b) (a * green b) (a * blue b)

data Result = Result {
    sd :: Float,
    emissive :: Color,
    reflectivity :: Float,
    eta :: Float,
    absorption :: Color
}

(.|.) :: Result -> Result -> Result
a .|. b = if sd a < sd b then a else b

(.&.) :: Result -> Result -> Result
a .&. b = if sd a > sd b then a else b

(.-.) :: Result -> Result -> Result
a .-. b = Result (if sd a > -sd b then sd a else -sd b) (emissive a) (reflectivity a) (eta a) (absorption a)

epsilon :: Float
epsilon = 1e-6

reflect :: Float -> Float -> Float -> Float -> (Float, Float)
reflect ix iy nx ny = (ix - idotn2 * nx, iy - idotn2 * ny) where
    idotn2 = (ix * nx + iy * ny) * 2.0

gradient :: Float -> Float -> (Float, Float)
gradient x y = (
    (sd (scene (x + epsilon) y) - sd (scene (x - epsilon) y)) * (0.5 / epsilon), 
    (sd (scene x (y + epsilon)) - sd (scene x (y - epsilon))) * (0.5 / epsilon))

refract :: Float -> Float -> Float -> Float -> Float -> Maybe (Float, Float)
refract ix iy nx ny eta = let
    idotn = ix * nx + iy * ny
    k = 1.0 - eta * eta * (1.0 - idotn * idotn) in
        if k < 0.0 then Nothing
        else let a = eta * idotn + sqrt k in 
            Just (eta * ix - a * nx, eta * iy - a * ny)

fresnel :: Float -> Float -> Float -> Float -> Float
fresnel cosi cost etai etat = (rs * rs + rp * rp) * 0.5 where
    rs = (etat * cosi - etai * cost) / (etat * cosi + etai * cost)
    rp = (etai * cosi - etat * cost) / (etai * cosi + etat * cost)

schlick :: Float -> Float -> Float -> Float -> Float
schlick cosi cost etai etat = r0 + (1.0 - r0) * (a ^ 5) where
    r0 = ((etai - etat) / (etai + etat)) ^ 2
    a = 1.0 - (if etai < etat then cosi else cost)

beerLambert :: Color -> Float -> Color
beerLambert a d = Color (exp (-red a * d)) (exp (-green a * d)) (exp (-blue a * d))

scene :: Float -> Float -> Result
scene x y = a .|. b where
    a = Result (circleSDF x y 0.5 (-0.2) 0.1) (Color 10.0 10.0 10.0) 0.0 0.0 black
    b = Result (ngonSDF x y 0.5 0.5 0.25 5.0) black 0.0 1.5 (Color 4.0 4.0 1.0)


trace :: Float -> Float -> Float -> Float -> Int -> Color
trace ox oy dx dy depth = traceImpl 0 1e-3 where
    traceImpl i t = if i < maxStep && t < maxDistance
        then let 
            x = ox + dx * t
            y = oy + dy * t
            r = scene x y in
            if sd r * sign < epsilon then let
                sum = emissive r in
                if depth < maxDepth && (reflectivity r > 0.0 || eta r > 0.0)
                    then let
                        refl = reflectivity r
                        (nx, ny) = gradient x y
                        nx' = sign * nx
                        ny' = sign * ny 
                        reflectProc s r = if r > 0.0
                            then let (rx, ry) = reflect dx dy nx' ny' in
                                s #+# (r *# trace (x + nx' * bias) (y + ny' * bias) rx ry (depth + 1))
                            else s in
                        if eta r > 0.0 then
                            case refract dx dy nx' ny' (if sign < 0.0 then eta r else 1.0 / eta r) of
                                Just (rx, ry) -> let
                                    cosi = -(dx * nx + dy * ny)
                                    cost = -(rx * nx + ry * ny)
                                    refl' = if sign < 0.0 
                                        then fresnel cosi cost (eta r) 1.0 
                                        else fresnel cosi cost 1.0 (eta r)
                                    sum' = sum #+# ((1.0 - refl') *# 
                                        trace (x - nx' * bias) (y - ny' * bias) rx ry (depth + 1))
                                    in
                                        reflectProc sum' refl' #*# beerLambert (absorption r) t
                                Nothing -> reflectProc sum 1.0 #*# beerLambert (absorption r) t
                        else reflectProc sum refl #*# beerLambert (absorption r) t
                    else sum #*# beerLambert (absorption r) t
            else traceImpl (i + 1) (t + sd r * sign)
        else black
    sign = if sd (scene ox oy) > 0.0 then 1.0 else -1.0
    maxStep = 64
    maxDistance = 5.0
    maxDepth = 3
    bias = 1e-4

sample :: Float -> Float -> Color
sample x y = (foldl (#+#) black $ map traceFunc [0..n - 1])  #* (1.0 / n) where
    traceFunc i = trace x y (cos $ a i) (sin $ a i) 0
    a i = 2 * pi * i / n
    n = 256 * 4


generateBeerLambertColorImage :: Image PixelRGB8
generateBeerLambertColorImage = generateImage pixelFunc width height where
    pixelFunc x y = PixelRGB8 r g b where
        fx = fromIntegral x / fromIntegral width
        fy = fromIntegral y / fromIntegral height
        color = sample fx fy
        r = floor $ min (red color * 255.0) 255.0
        g = floor $ min (green color * 255.0) 255.0
        b = floor $ min (blue color * 255.0) 255.0
    height = 512
    width = 512

dumpBeerLambertColorFile :: IO ()
dumpBeerLambertColorFile = writePng "img/beerLambertColor.png" generateBeerLambertColorImage
