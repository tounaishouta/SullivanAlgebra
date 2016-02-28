module Cohomology
    ( SullivanAlgebra
    , Polynomial
    , Degree
    , readSullivanAlgebra
    , cohomology
    ) where

import Data.Array

import SullivanAlgebra
import Matrix

type Space = Array Index Term

space :: [Term] -> Space
space ts = listArray (1, length ts) ts

dimension :: Space -> Dimension
dimension = snd . bounds

basis :: Space -> [Term]
basis = elems

fromVecor :: Space -> Vector Integer -> Polynomial
fromVecor spc vec = Sum [ c :* (spc ! i) | (i, c) <- vec ]

toVector :: Space -> Polynomial -> Vector Integer
toVector spc poly = [ (spc ? t, c) | c :* t <- summands poly ]

(?) :: Space -> Term -> Index
spc ? t = bsearch $ bounds spc where
    bsearch (a, b)
        | a == b      = a
        | spc ! c < t = bsearch (c + 1, b)
        | otherwise   = bsearch (a, c)
        where c = (a + b) `div` 2

cohomology :: SullivanAlgebra -> [[Polynomial]]
cohomology alg = [ map (fromVecor cochain) $ quotient cocycle coboundary
                 | (cochain, coboundary, cocycle)
                   <- zip3 cochains (zero : coboundaries) cocycles
                 ]
    where

        cochains :: [Space]
        cochains = map (space . terms alg) [0 .. ]

        coboundaries :: [Basis Integer]
        cocycles     :: [Basis Integer]
        (coboundaries, cocycles) = unzip $ map (decompose . delta) [0 .. ]

        zero :: Basis Integer
        zero = []

        delta :: Degree -> Matrix Integer
        delta deg = matrix (dimension tgt)
                           (map (toVector tgt . differential) $ basis src)
            where src = cochains !! deg
                  tgt = cochains !! (deg + 1)
