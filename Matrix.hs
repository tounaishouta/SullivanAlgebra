module Matrix
    ( Matrix
    , Dimension
    , Vector
    , Index
    , Basis
    , matrix
    , decompose
    , quotient
    ) where

import Control.Applicative ((<$>))
import Data.Maybe          (fromMaybe)

data Matrix c  = Matrix { height :: Dimension, columns :: [Vector c] }
type Dimension = Int
type Vector c  = [(Index, c)]
type Index     = Int
type Basis c   = [Vector c]

matrix :: Dimension -> [Vector c] -> Matrix c
matrix ht cols = Matrix { height = ht, columns = cols }

isZero :: Vector c -> Bool
isZero = null

firstIndex :: Vector c -> Index
firstIndex = fst . head

class (Eq c, Num c) => Sweep c where
    sweep :: c -> c -> (c, c)

sweepIntegral ::Integral c => c -> c -> (c, c)
sweepIntegral x y = (- y `div` g, x `div` g) where
    g = gcd x y

instance Sweep Integer where
    sweep = sweepIntegral

decompose :: Sweep c => Matrix c -> (Basis c, Basis c)
decompose mat = (img, ker) where
    img = [ take' ht vec | vec <- img' ]
    ker = [ drop' ht vec | vec <- ker' ]
    (img', ker') = span ((<= ht) . firstIndex) vs
    vs = reduce [ col ++ [(ht + i, 1)] | (i, col) <- zip [1 .. ] (columns mat) ]
    ht = height mat
    take' n = takeWhile ((<= n) . fst)
    drop' n v = [ (i - n, c) | (i, c) <- dropWhile ((<= n) . fst) v ]

quotient :: Sweep c => Basis c -> Basis c -> Basis c
quotient []       _  = []
quotient (v : vs) ws = case addMaybe v ws of
                            Nothing  -> quotient vs ws
                            Just ws' -> v : quotient vs ws'

reduce :: Sweep c => [Vector c] -> Basis c
reduce = foldr add []

add :: Sweep c => Vector c -> Basis c -> Basis c
add v ws = fromMaybe ws $ addMaybe v ws

addMaybe :: Sweep c => Vector c -> Basis c -> Maybe (Basis c)
addMaybe v ws
    | isZero v  = Nothing
    | null ws   = Just [v]
    | i < j     = Just (v : ws)
    | i > j     = (w :) <$> addMaybe v ws'
    | otherwise = (w :) <$> addMaybe v' ws'
    where w : ws' = ws
          (i, a) : _ = v
          (j, b) : _ = w
          (c, d) = sweep a b
          v' = c *. v .+. d *. w

infixr 7 *.
(*.) :: (Eq c, Num c) => c -> Vector c -> Vector c
a *. v = filter ((/= 0) . snd) [ (i, a * c) | (i, c) <- v ]

infixr 6 .+.
(.+.) :: (Eq c, Num c) => Vector c -> Vector c -> Vector c
v .+. w
    | isZero v  = w
    | isZero w  = v
    | i < j     = (i, a) : v' .+. w
    | i > j     = (j, b) : v .+. w'
    | c == 0    = v' .+. w'
    | otherwise = (i, c) : v' .+. w'
    where (i, a) : v' = v
          (j, b) : w' = w
          c = a + b
