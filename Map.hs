module Map
    ( Map
    , lookup
    , hasKey
    , (!?)
    , empty
    , insert
    , fromList
    , assocs
    , keys
    , elems
    , size
    ) where

import Prelude hiding (lookup)
import Data.Maybe

data Map k v = Empty
             | Leaf k v
             | Node2 k (Map k v) (Map k v)
             | Node3 k (Map k v) (Map k v) (Map k v)

firstKey :: Map k v -> k
firstKey Empty           = error "Map.firstKey: Empty"
firstKey (Leaf k _)      = k
firstKey (Node2 k _ _)   = k
firstKey (Node3 k _ _ _) = k

node2 :: Map k v -> Map k v -> Map k v
node2 ml mr = Node2 (firstKey ml) ml mr

node3 :: Map k v -> Map k v -> Map k v -> Map k v
node3 ml mc mr = Node3 (firstKey ml) ml mc mr

lookup :: Ord k => Map k v -> k -> Maybe v
lookup Empty _        = Nothing
lookup (Leaf k' v) k
    | k == k'         = Just v
    | otherwise       = Nothing
lookup (Node2 _ ml mr) k
    | k < firstKey ml = Nothing
    | k < firstKey mr = lookup ml k
    | otherwise       = lookup mr k
lookup (Node3 _ ml mc mr) k
    | k < firstKey ml = Nothing
    | k < firstKey mc = lookup ml k
    | k < firstKey mr = lookup mc k
    | otherwise       = lookup mr k

hasKey :: Ord k => Map k v -> k -> Bool
hasKey m k = isJust $ lookup m k

(!?) :: Ord k => Map k v -> k -> v
(!?) m k = fromMaybe (error "can not find key") $ lookup m k

empty :: Map k v
empty = Empty

insert :: Ord k => Map k v -> (k, v) -> Map k v
insert m kv = case insert' m kv of
              Left m'        -> m'
              Right (ml, mr) -> node2 ml mr

insert' :: Ord k => Map k v -> (k, v) -> Either (Map k v) (Map k v, Map k v)
insert' Empty (k, v) = Left $ Leaf k v
insert' (Leaf k' v') (k, v) = case compare k k' of
                              LT -> Right (Leaf k v, Leaf k' v')
                              EQ -> Left $ Leaf k v
                              GT -> Right (Leaf k' v', Leaf k v)
insert' (Node2 _ ml mr) (k, v)
    | k < firstKey mr = case insert' ml (k, v) of
                        Left ml'         -> Left $ node2 ml' mr
                        Right (mll, mlr) -> Left $ node3 mll mlr mr
    | otherwise       = case insert' mr (k, v) of
                        Left mr'         -> Left $ node2 ml mr'
                        Right (mrl, mrr) -> Left $ node3 ml mrl mrr
insert' (Node3 _ ml mc mr) (k, v)
    | k < firstKey mc = case insert' ml (k, v) of
                        Left ml'         -> Left $ node3 ml' mc mr
                        Right (mll, mlr) -> Right (node2 mll mlr, node2 mc mr)
    | k < firstKey mr = case insert' mc (k, v) of
                        Left mc'         -> Left $ node3 ml mc' mr
                        Right (mcl, mcr) -> Right (node2 ml mcl, node2 mcr mr)
    | otherwise       = case insert' mr (k, v) of
                        Left mr'         -> Left $ node3 ml mc mr'
                        Right (mrl, mrr) -> Right (node2 ml mc, node2 mrl mrr)

fromList :: Ord k => [(k, v)] -> Map k v
fromList = foldl insert empty

assocs :: Map k v -> [(k, v)]
assocs Empty              = []
assocs (Leaf k v)         = [(k, v)]
assocs (Node2 _ ml mr)    = concatMap assocs [ml, mr]
assocs (Node3 _ ml mc mr) = concatMap assocs [ml, mc, mr]

keys :: Map k v -> [k]
keys = map fst . assocs

elems :: Map k v -> [v]
elems = map snd . assocs

size :: Map k v -> Int
size = length . assocs
