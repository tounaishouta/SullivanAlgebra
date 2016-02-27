module Matrix
    ( Matrix
    , Vector(Vector)
    , Basis
    , Dimension
    , Index
    , matrix
    , decompose
    , quotient
    , summands
    ) where

import Data.Maybe (fromMaybe)

data Matrix c  = Matrix { height :: Dimension, columns :: [Vector c] }
type Dimension = Int
data Vector c  = Vector { summands :: [(Index, c)] }
type Index     = Int
type Basis c   = [Vector c]

matrix :: Dimension -> [Vector c] -> Matrix c
matrix ht cols = Matrix { height = ht, columns = cols }

isZero :: Vector c -> Bool
isZero = null . summands

firstIndex :: Vector c -> Index
firstIndex = fst . head . summands

firstCoefficient :: Vector c -> c
firstCoefficient = snd . head . summands

standardBasis :: Num c => Index -> Vector c
standardBasis i = Vector [(i, 1)]

take' :: Dimension -> Vector c -> Vector c
take' n = Vector . takeWhile ((<= n) . fst) . summands

drop' :: Dimension -> Vector c -> Vector c
drop' n = Vector . map (mapFst (subtract n)) . dropWhile ((<= n) . fst) . summands

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)

infixr 6 .+.
(.+.) :: (Eq c, Num c) => Vector c -> Vector c -> Vector c
v .+. w = Vector $ merge (summands v) (summands w) where
    merge xs ys
        | null xs    = ys
        | null ys    = xs
        | i < j      = (i, a) : merge xs' ys
        | i > j      = (j, b) : merge xs ys'
        | a + b == 0 = merge xs' ys'
        | otherwise  = (i, a + b) : merge xs' ys'
        where (i, a) : xs' = xs
              (j, b) : ys' = ys

infixr 7 *.
(*.) :: (Eq c, Num c) => c -> Vector c -> Vector c
c *. v = Vector $ filter ((/= 0) . snd) [ (i, c * a) | (i, a) <- summands v ]

decompose :: Sweep c => Matrix c -> (Basis c, Basis c)
decompose (Matrix ht vecs) = (ker, im) where
    ker         = [ drop' ht v | v <- ker' ]
    im          = [ take' ht v | v <- im' ]
    (im', ker') = span ((<= ht) . firstIndex)
                $ reduce [ v .+. standardBasis (ht + i)
                         | (i, v) <- zip [1 .. ] vecs
                         ]

quotient :: Sweep c => Basis c -> Basis c -> Basis c
quotient vs ws
    | null vs   = []
    | otherwise = case addMaybe v ws of
                  Nothing  -> quotient vs' ws
                  Just ws' -> v : quotient vs' ws'
    where v : vs' = vs


reduce :: Sweep c => [Vector c] -> Basis c
reduce = foldr add []

add :: Sweep c => Vector c -> Basis c -> Basis c
add v ws = fromMaybe ws $ addMaybe v ws

addMaybe :: Sweep c => Vector c -> Basis c -> Maybe (Basis c)
addMaybe v ws
    | isZero v                    = Nothing
    | null ws                     = Just [v]
    | firstIndex v < firstIndex w = Just $ v : ws
    | firstIndex v > firstIndex w = case addMaybe v ws' of
                                    Just xs -> Just $ w : xs
                                    Nothing -> Nothing
    | otherwise                   = case addMaybe v' ws' of
                                    Just xs -> Just $ w : xs
                                    Nothing -> Nothing
    where w : ws'      = ws
          (c, d) = sweep (firstCoefficient w) (firstCoefficient v)
          v'           = c *. w .+. d *. v

class (Eq c, Num c) => Sweep c where
    sweep :: c -> c -> (c, c)

sweepIntegral :: Integral c => c -> c -> (c, c)
sweepIntegral 0 _ = (1, 0)
sweepIntegral _ 0 = (0, 1)
sweepIntegral x y = (y `div` g, - x `div` g) where
    g =gcd x y

instance Sweep Integer where
    sweep = sweepIntegral
