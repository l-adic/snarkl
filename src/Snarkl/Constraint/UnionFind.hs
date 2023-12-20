{-# LANGUAGE NamedFieldPuns #-}

module Snarkl.Constraint.UnionFind
  ( root,
    unite,
    empty,
    extraOf,
    extras,
    UnionFind,
    -- exported for tests
    insert,
  )
where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)

data UnionFind node a = UnionFind
  { ids :: Map.Map node node,
    sizes :: Map.Map node Int,
    extras :: Map.Map node a
  }
  deriving (Show, Eq)

empty :: UnionFind node a
empty = UnionFind Map.empty Map.empty Map.empty

idOf :: (Ord node) => UnionFind node a -> node -> node
idOf (UnionFind {ids}) x = fromMaybe x $ Map.lookup x ids

sizeOf :: (Ord node) => UnionFind node a -> node -> Int
sizeOf (UnionFind {sizes}) x = fromMaybe 1 $ Map.lookup x sizes

extraOf :: (Ord node) => UnionFind node a -> node -> Maybe a
extraOf (UnionFind {extras}) x = Map.lookup x extras

root :: (Show a, Eq a, Ord node, Show node) => UnionFind node a -> node -> (node, UnionFind node a)
root uf x =
  let px = idOf uf x
   in if px == x
        then (x, uf)
        else
          let gpx = idOf uf px
              uf' = mergeExtras uf x gpx
           in root (uf' {ids = Map.insert x gpx (ids uf)}) px

mergeExtras :: (Show a, Eq a, Ord node, Show node) => UnionFind node a -> node -> node -> UnionFind node a
mergeExtras uf x y =
  case (Map.lookup x (extras uf), Map.lookup y (extras uf)) of
    (Nothing, Nothing) -> uf
    (Nothing, Just d) -> uf {extras = Map.insert x d (extras uf)}
    (Just c, Nothing) -> uf {extras = Map.insert y c (extras uf)}
    (Just c, Just d) ->
      if c == d
        then uf
        else
          failWith $
            ErrMsg
              ( "in UnionFind, extra data doesn't match:\n  "
                  ++ show (x, c)
                  ++ " != "
                  ++ show (y, d)
              )

-- | Unify x with y.  On ties, prefer smaller variables. This is just
-- a heuristic that biases toward pinned variables, many of which are
-- low-numbered input vars. This way, we avoid introducing pinned
-- eqns. in some cases.
unite :: (Show a, Eq a, Ord node, Show node) => UnionFind node a -> node -> node -> UnionFind node a
unite uf x y
  | x < y =
      go x y
  | x > y =
      go y x
  | otherwise =
      uf
  where
    -- Left-biased: if size x == size y, prefer x as root.
    go x0 y0 =
      let (rx, uf2) = root uf x0
          (ry, uf') = root uf2 y0
          sz_rx = sizeOf uf' rx
          sz_ry = sizeOf uf' ry
       in if sz_rx >= sz_ry
            then
              uf'
                { ids = Map.insert y0 rx (ids uf'),
                  sizes = Map.insert x0 (sz_rx + sz_ry) (sizes uf')
                }
            else
              uf'
                { ids = Map.insert x0 ry (ids uf'),
                  sizes = Map.insert y0 (sz_rx + sz_ry) (sizes uf')
                }

-- | Bind variable 'x' to 'c'.
insert :: (Ord node, Show node, Eq a, Show a) => node -> a -> UnionFind node a -> UnionFind node a
insert x c uf =
  do
    let (rx, uf') = root uf x
    uf' {extras = Map.insert rx c (extras uf')}
