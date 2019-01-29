import Light2D.Basic
import Light2D.CSG
import Light2D.Shapes
import Light2D.Reflection
import Light2D.Refraction
import Debug.Trace

main :: IO ()
main = foldl (>>) (putStrLn "Start dumping images...") $ 
    map (uncurry $ trace . (++ " part...")) [
        ("Basic",       dumpBasicFile), 
        ("CSG",         dumpCSGFile), 
        ("Shapes",      dumpShapesFile), 
        ("Reflection",  dumpReflectionFile), 
        ("Refraction",  dumpRefractionFile)
    ]
