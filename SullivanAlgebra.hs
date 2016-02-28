module SullivanAlgebra
    ( SullivanAlgebra
    , Degree
    , Polynomial(Sum)
    , Monomial((:*))
    , Term
    , summands
    , differential
    , terms
    , readSullivanAlgebra
    ) where

import Data.List        (intercalate)
import Data.Maybe       (fromMaybe)
import Text.Regex.Posix ((=~))

data SullivanAlgebra = SullivanAlgebra { variables :: [Variable] }
data Variable        = Variable Index String Degree Polynomial
type Index           = Int
type Degree          = Int
data Polynomial      = Sum { summands :: [Monomial] }
data Monomial        = Integer :* Term
data Term            = Product { factors :: [Power] }
data Power           = Variable :^ Exponent
type Exponent        = Int

infix 7 :*
infix 8 :^

class IsPolynomial a where
    toPolynomial :: a -> Polynomial

instance IsPolynomial Polynomial where
    toPolynomial = id

instance IsPolynomial Monomial where
    toPolynomial (0 :* _) = Sum []
    toPolynomial m        = Sum [m]

instance IsPolynomial Integer where
    toPolynomial n = toPolynomial $ n :* Product []

instance IsPolynomial Term where
    toPolynomial t = toPolynomial $ 1 :* t

instance IsPolynomial Power where
    toPolynomial (_ :^ 0) = toPolynomial $ Product []
    toPolynomial p        = toPolynomial $ Product [p]

instance IsPolynomial Variable where
    toPolynomial v = toPolynomial $ v :^ 1

class HasDegree a where
    degree :: a -> Degree

instance HasDegree Term where
    degree = sum . map degree . factors

instance HasDegree Power where
    degree (v :^ n) = n * degree v

instance HasDegree Variable where
    degree (Variable _ _ deg _) = deg

class Differential a where
    differential :: a -> Polynomial

instance Differential Polynomial where
    differential = sum . map differential . summands

instance Differential Monomial where
    differential (c :* t) = fromInteger c * differential t

instance Differential Term where
    differential (Product [])       = 0
    differential (Product (x : ys)) = differential x
                                    * toPolynomial (Product ys)
                                    + toPolynomial ((-1) ^ degree x :: Integer)
                                    * toPolynomial x
                                    * differential (Product ys)

instance Differential Power where
    differential (v :^ n) = toPolynomial (fromIntegral n :: Integer)
                          * toPolynomial (v :^ (n - 1))
                          * differential v

instance Differential Variable where
    differential (Variable _ _ _ diff) = diff

instance Num Polynomial where

    p + q = Sum $ merge (summands p) (summands q) where

        merge :: [Monomial] -> [Monomial] -> [Monomial]
        merge xs ys
            | null xs    = ys
            | null ys    = xs
            | t < s      = a :* t : merge xs' ys
            | t > s      = b :* s : merge xs ys'
            | a + b == 0 = merge xs' ys'
            | otherwise  = (a + b) :* t : merge xs' ys'
            where a :* t : xs' = xs
                  b :* s : ys' = ys

    p * q = sum [ toPolynomial $ times x y | x <- summands p, y <- summands q ] where

        times :: Monomial -> Monomial -> Monomial
        times (a :* t) (b :* s) = foldr times' ((a * b) :* s) (factors t)

        times' :: Power -> Monomial -> Monomial
        times' (v :^ n) (c :* Product xs)
            -- | c == 0         = 0 :* error "do not evaluate me!"
            | null xs        = c :* Product [v :^ n]
            | v < w          = c :* Product (v :^ n : xs)
            | v > w          = times' (w :^ m) (times' (v :^ n) (c' :* Product xs'))
            | odd (degree v) = 0 :* Product [] -- まずいがやむなし
            | otherwise      = c :* Product (v :^ (n + m) : xs')
            where w :^ m : xs' = xs
                  c'           = (-1) ^ (degree (v :^ n) * degree (w :^ m)) * c

    negate = ((-1) *)

    fromInteger = toPolynomial

    abs    = error "Polynomial does not support `abs`"
    signum = error "Polynomial does not support `signum`"

instance Eq Term where
    t == s = factors t == factors s

instance Eq Power where
    (v :^ n) == (w :^ m) = v == w && n == m

instance Eq Variable where
    Variable idx _ _ _ == Variable idx' _ _ _ = idx == idx'

instance Ord Term where
    compare t s = compare (factors t) (factors s)

instance Ord Power where
    compare (v :^ n) (w :^ m) = case compare v w of
                                     LT -> LT
                                     GT -> GT
                                     EQ -> compare m n -- 逆であってる

instance Ord Variable where
    compare (Variable idx _ _ _) (Variable idx' _ _ _) = compare idx idx'

terms :: SullivanAlgebra -> Degree -> [Term]
terms alg = map (Product . operate) . terms' (variables alg) where

    terms' :: [Variable] -> Degree -> [[Variable]]
    terms' _ 0      = [[]]
    terms' vs deg
        | deg < 0   = []
        | null vs   = []
        | even degv = map (v :) (terms' vs (deg - degv)) ++ terms' vs' deg
        | otherwise = map (v :) (terms' vs' (deg - degv)) ++ terms' vs' deg
        where v : vs' = vs
              degv    = degree v

    operate :: [Variable] -> [Power]
    operate [] = []
    operate vs = w :^ length ws : operate xs where
        w = head vs
        (ws, xs) = span (== w) vs

instance Show Polynomial where
    show (Sum []) = "0"
    show (Sum ms) = intercalate " + " $ map show ms

instance Show Monomial where
    show (c :* Product []) = show c
    show (1 :* t)          = show t
    show (c :* t)          = unwords [show c, show t]

instance Show Term where
    show (Product []) = "1"
    show (Product ps) = unwords $ map show ps

instance Show Power where
    show (v :^ 1) = show v
    show (v :^ n) = intercalate "^" [show v, show n]

instance Show Variable where
    show (Variable _ name _ _) = name

readSullivanAlgebra :: String -> SullivanAlgebra
readSullivanAlgebra input = alg where

    alg :: SullivanAlgebra
    alg = SullivanAlgebra $ zipWith read' [1 .. ] (lines input)

    read' :: Index -> String -> Variable
    read' idx str = Variable idx name (read deg) (readPolynomial diff) where
        [name, deg, diff] = splitOn '\t' str

    splitOn :: Char -> String -> [String]
    splitOn delim xs = case break (== delim) xs of
                            (ys, [])     -> [ys]
                            (ys, _ : zs) -> ys : splitOn delim zs

    readPolynomial :: String -> Polynomial
    readPolynomial "0" = 0
    readPolynomial str = sum $ map readTerm $ splitOn '+' str

    readTerm :: String -> Polynomial
    readTerm = product . map readFactor . words

    readFactor :: String -> Polynomial
    readFactor str
        | isInteger str = fromInteger (read str)
        | otherwise         = case splitOn '^' str of
                                   [v]    -> toPolynomial $ readVariable v
                                   [v, n] -> toPolynomial $ readVariable v :^ read n
                                   _      -> error $ "Parse error at `" ++ str ++ "`"

    readVariable :: String -> Variable
    readVariable str = fromMaybe (error $ "Illegal variable name `" ++ str ++ "`")
                                 (lookup str [ (show v, v) | v <- variables alg ])

    isInteger :: String -> Bool
    isInteger = (=~ "^-?[0-9]+$")
