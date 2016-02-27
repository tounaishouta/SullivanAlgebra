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

data Matrix c  = Matrix { height :: Dimension, columns :: [Vector c] }
type Dimension = Int
data Vector c  = Vector { summands :: [(Index, c)] }
type Index     = Int
type Basis c   = [Vector c]

matrix :: Dimension -> [Vector c] -> Matrix c
matrix ht cols = Matrix { height = ht, columns = cols }

decompose :: Matrix c -> (Basis c, Basis c)
decompose = undefined

quotient :: Basis c -> Basis c -> Basis c
quotient = undefined
