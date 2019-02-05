import Light2D.Basic
import Light2D.CSG
import Light2D.Shapes
import Light2D.Reflection
import Light2D.Refraction
import Light2D.Fresnel
import Light2D.BeerLambert
import Light2D.Dispersion
import Debug.Trace

main :: IO ()
main = foldl (>>) (putStrLn "Start dumping images...") $ 
    map (uncurry $ trace . (++ " part...")) [
        ("Basic",       dumpBasicFile), 
        ("CSG",         dumpCSGFile), 
        ("Shapes",      dumpShapesFile), 
        ("Reflection",  dumpReflectionFile), 
        ("Refraction",  dumpRefractionFile),
        ("Fresnel",     dumpFresnelFile),
        ("BeerLambert", dumpBeerLambertFile),
        ("Dispersion",  dumpDispersiontFile)
    ]
