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
                 }
                 where dim = length ts

fromVector :: Space -> Vector Integer -> Polynomial
fromVector spc vec = Polynomial [ c :* (_array spc ! i) | (i, c) <- summands vec ]

toVector :: Space -> Polynomial -> Vector Integer
toVector spc poly =  Vector [ (_map spc !? t, c) | c :* t <- monomials poly ]

cohomology :: SullivanAlgebra -> Degree -> [Polynomial]
cohomology alg deg = map (fromVector cochain) $ quotient cocycle coboundary where

    cochain :: Space
    cochain = cochains !! deg

    cocycle :: Basis Integer
    cocycle = cocycles !! deg

    coboundary :: Basis Integer
    coboundary = coboundaries !! deg

    cochains :: [Space]
    cochains = map (space . terms) [0 .. ]

    terms :: Degree -> [Term]
    terms = undefined alg

    cocycles     :: [Basis Integer]
    coboundaries :: [Basis Integer]
    (_ : cocycles, coboundaries) = unzip $ map (decompose . differentialMatrix)  [-1 .. ]

    differentialMatrix :: Degree -> Matrix Integer
    differentialMatrix = undefined alg toVector
