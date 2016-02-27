import Control.Applicative ((<$>))
import System.Environment  (getArgs)

import SullivanAlgebra
import Cohomology

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path]      -> main' path [0 .. ]
        [path, deg] -> main' path [0 .. read deg]
        _           -> putStrLn "Usage: ./Main {path} [{maxdeg}]"

main' :: FilePath -> [Degree] -> IO ()
main' path degs = do
    alg <- readSullivanAlgebra <$> readFile path
    putStrLn $ unlines
        [ unlines (map show basis)
          ++ "dim H^" ++ show deg ++ " = " ++ show (length basis)
        | deg <- degs
        , let basis = cohomology alg deg
        ]
