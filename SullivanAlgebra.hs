module SullivanAlgebra
    ( Degree
    , readSullivanAlgebra
    , cohomology
    ) where

import Prelude hiding (exponent)

import Data.List        (intercalate)
import Data.Maybe       (fromJust)
import Text.Regex.Posix ((=~))

data SullivanAlgebra = SullivanAlgebra { variables :: [Variable] }
data Variable        = Variable { index :: ID
                                , name  :: String
                                , _deg  :: Degree
                                , _diff :: Polynomial
                                }
type ID              = Int
type Degree          = Int
data Polynomial      = Polynomial { monomials :: [Monomial] }
data Monomial        = Integer :* Term
type Term            = [Power]
data Power           = Variable :^ Multiplicity
type Multiplicity    = Int

infix 7 :*
infix 8 :^

coefficient :: Monomial -> Integer
coefficient (c :* _) = c

-- term :: Monomial -> [Power]
-- term (_ :* t) = t

base :: Power -> Variable
base (v :^ _) = v

exponent :: Power -> Multiplicity
exponent (_ :^ n) = n

fromMonomial :: Monomial -> Polynomial
fromMonomial m
    | coefficient m == 0 = Polynomial []
    | otherwise          = Polynomial [m]

fromPower :: Power -> Monomial
fromPower x
    | exponent x == 0 = 1 :* []
    | otherwise       = 1 :* [x]

class HasDegree a where
    degree :: a -> Degree

instance HasDegree Monomial where
    degree (a :* xs)
        | a == 0    = error "`degree` not defined"
        | otherwise = sum $ map degree xs

instance HasDegree Power where
    degree (v :^ n) = n * degree v

instance HasDegree Variable where
    degree = _deg

class Differential a where
    differential :: a -> Polynomial

instance Differential Polynomial where
    differential = sum . map differential . monomials

instance Differential Monomial where
    differential (_ :* [])
        = 0
    differential (c :* (x : ys))
        = differential x * fromMonomial (c :* ys)
        + fromMonomial (fromPower x) * differential (c :* ys)

instance Differential Power where
    differential (v :^ n)
        | n == 1    = differential v
        | otherwise = fromInteger (fromIntegral n)
                    * fromMonomial (fromPower (v :^ (n - 1)))
                    * differential v

instance Differential Variable where
    differential = _diff

instance Num Polynomial where

    fromInteger = fromMonomial . fromInteger

    p + q = Polynomial $ merge (monomials p) (monomials q) where
        merge xs ys
            | null xs    = ys
            | null ys    = xs
            | t < s      = (a :* t) : merge xs' ys
            | t > s      = (b :* s) : merge xs ys'
            | a + b == 0 = merge xs' ys'
            | otherwise  = ((a + b) :* t) : merge xs' ys'
            where (a :* t) : xs' = xs
                  (b :* s) : ys' = ys

    negate = ((-1) *)

    p * q = sum [ fromMonomial (m * n) | m <- monomials p, n <- monomials q ]

    abs    = error "Polynomial does not support `abs`"
    signum = error "Polynomial does not support `signum`"

instance Num Monomial where

    fromInteger = (:* [])

    (a :* xs) * (b :* ys) = foldr times ((a * b) :* ys) xs where

        infixr 6 `times`
        times :: Power -> Monomial -> Monomial
        z `times` (c :* ws)
            | null ws   = c :* [z]
            | otherwise = case compare (base z) (base w) of
                          LT -> c :* (z : ws)
                          GT -> w `times` z `times` (c' :* ws')
                          EQ -> if vanish then 0
                                          else c :* (zw : ws')
            where w : ws' = ws
                  c'      = (-1) ^ (degree z * degree w) * c
                  zw      = base z :^ (exponent z + exponent w)
                  vanish  = odd $ degree $ base z

    (+)    = error "Monomial does not support `(+)`"
    negate = error "Monomial does not support `negate`"
    abs    = error "Monomial does not support `abs`"
    signum = error "Monomial does not support `signum`"

instance Eq Power where
    x == y = base x == base y && exponent x == exponent y

instance Eq Variable where
    v == w = index v == index w

instance Ord Power where
    compare x y = case compare (base x) (base y) of
                  LT -> LT
                  EQ -> compare (exponent y) (exponent x)
                  GT -> GT

instance Ord Variable where
    compare v w = compare (index v) (index w)

instance Show Polynomial where
    show (Polynomial []) = "0"
    show (Polynomial ms) =  intercalate " + " $ map show ms

instance Show Monomial where
    show (c :* []) = show c
    show (1 :* xs) = unwords $ map show xs
    show (c :* xs) = unwords $ show c : map show xs

instance Show Power where
    show (v :^ 1) = show v
    show (v :^ n) = show v ++ "^" ++ show n

instance Show Variable where
    show = name

readSullivanAlgebra :: String -> SullivanAlgebra
readSullivanAlgebra input = alg where

    alg :: SullivanAlgebra
    alg = SullivanAlgebra $ zipWith readLine [1 .. ] (lines input)

    readLine :: ID -> String -> Variable
    readLine i str = Variable i nam (read deg) (readPolynomial poly) where
        [nam, deg, poly] = splitOn '\t' str

    readPolynomial :: String -> Polynomial
    readPolynomial "0" = 0
    readPolynomial str = sum $ map fromMonomial monos where
        monos = map readMonomial $ splitOn '+' str

    readMonomial :: String -> Monomial
    readMonomial = product . map readFactor . words

    readFactor :: String -> Monomial
    readFactor str
        | isInteger str = fromInteger $ read str
        | otherwise     = case splitOn '^' str of
                          [v]    -> fromPower $ readVariable v :^ 1
                          [v, n] -> fromPower $ readVariable v :^ read n
                          _      -> error $ "Parse error at: " ++ str

    readVariable :: String -> Variable
    readVariable = fromJust . flip lookup [ (name v, v) | v <- variables alg ]

    splitOn :: Eq a => a -> [a] -> [[a]]
    splitOn delim xs = case break (== delim) xs of
                       (ys, [])     -> [ys]
                       (ys, _ : zs) -> ys : splitOn delim zs

    isInteger :: String -> Bool
    isInteger = (=~ "^-?[0-9]+$")

cohomology :: SullivanAlgebra -> Degree -> [Polynomial]
cohomology = undefined
-- cohomology alg 0 = [ differential v | v <- variables alg ]
-- cohomology _   _ = []
