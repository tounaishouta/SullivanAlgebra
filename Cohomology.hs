module Cohomology
    ( cohomology
    ) where

import Data.Array

import SullivanAlgebra
import Map (Map, (!?))
import qualified Map
import Matrix

data Space = Space { dimension :: Dimension
                   , _array    :: Array Index Term
                   , _map      :: Map Term Index
                   }

space :: [Term] -> Space
space ts = Space { dimension = dim
                 , _array    = listArray (1, dim) ts
                 , _map      = Map.fromList $ zip ts [1 .. dim]
                 } where
    dim = length ts

fromVector :: Space -> Vector Integer -> Polynomial
fromVector spc vec = Polynomial [ c :* (_array spc ! i)
                                | (i, c) <- summands vec
                                ]

toVector :: Space -> Polynomial -> Vector Integer
toVector spc poly = Vector [ (_map spc !? t, c) | c :* t <- monomials poly ]

basis :: Space -> [Term]
basis = elems . _array

cohomology :: SullivanAlgebra -> [[Polynomial]]
cohomology alg = [ map (fromVector cochain) $ quotient cocycle coboundary
                 | (cochain, cocycle, coboundary)
                   <- zip3 cochains cocycles (zero : coboundaries)
                 ] where

    cochains :: [Space]
    cochains = map (space . terms (variables alg)) [0 .. ]

    cocycles     :: [Basis Integer]
    coboundaries :: [Basis Integer]
    (cocycles, coboundaries) = unzip $ map (decompose . delta)  [0 .. ]

    zero :: Basis Integer
    zero = []

    delta :: Degree -> Matrix Integer
    delta deg = matrix (dimension tgt) [ toVector tgt $ differential (1 :* t)
                                       | t <- basis src
                                       ] where
        src = cochains !! deg
        tgt = cochains !! (deg + 1)
