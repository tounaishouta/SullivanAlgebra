import Control.Applicative ((<$>))
import System.Environment  (getArgs)

import Cohomology

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path]         -> main' path [0 .. ]
        [path, maxdeg] -> main' path [0 .. read maxdeg]
        _              -> putStrLn "Usage: ./Main {path} [{maxdeg}]"

main' :: FilePath -> [Degree] -> IO ()
main' path degs = do
    alg <- readSullivanAlgebra <$> readFile path
    putStrLn $ unlines
        [ unlines [ show deg ++ ":\t" ++ show poly | poly <- basis ]
          ++ "---- dim H^" ++ show deg ++ " = " ++ show (length basis)
        | (deg, basis) <- zip degs (cohomology alg)
        ]
