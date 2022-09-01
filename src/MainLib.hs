{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module MainLib
  ( run,
    Heap,
    MaxHeap,
    x,
  )
where

run :: String -> IO ()
run = putStrLn

x = [1, 2, 3, 4, 5] :: [Int]

y = fromList x :: MaxHeap Int

class Heap h where
  isEmpty :: (Ord a) => h a -> Bool
  insert :: (Ord a) => h a -> a -> h a
  height :: (Ord a) => h a -> Int
  fromList :: (Ord a) => [a] -> h a


data MaxHeap a
  = Empty
  | T a (MaxHeap a) (MaxHeap a)

-- deriving Show


instance Heap MaxHeap where
  isEmpty Empty = True
  isEmpty _ = False

  height Empty = 0
  height (T _ l r) = 1 + max (height l) (height r)

  insert Empty       v   = T v Empty Empty
  insert (T v l r) v'  =
    if v' > v
      then T v (insert l v')  r
      else T v l              (insert r v')

  fromList [] = Empty
  fromList as = foldl insert Empty as

instance (Show a) => Show (MaxHeap a) where
  show Empty = ""
  show (T v r l) = "\n " ++ show v ++ show r ++ show l

instance Functor MaxHeap where
  fmap _ Empty = Empty
  fmap f (T v l r) = T (f v) (fmap f l) (fmap f r)

data Direction = L | R

type Breadcrumbs = [Direction]

toList :: MaxHeap a -> [a]
toList Empty = []
toList (T v l r) = v : toList l ++ toList r

type Vert a = (String, a)
type Edge a = (Vert a, Vert a)

cVert :: String -> a -> Vert a
cVert str v = (str, v)

cEdge :: Vert a -> Vert a -> Edge a
cEdge v1 v2 = (v1, v2)


toVert :: (Show a) => a -> Vert a
toVert a = ("n" ++ show a, a)

toEdge :: (Show a) => a -> a -> Edge a
toEdge v1 v2 = (toVert v1, toVert v2)

toEdgeList :: (Show a) => MaxHeap a -> [Edge a]
toEdgeList Empty = []
toEdgeList (T v Empty         Empty) = []
toEdgeList (T v (T v' l r)    Empty) = toEdge v v' : toEdgeList l ++ toEdgeList r
toEdgeList (T v Empty         (T v' l r)) = toEdge v v' : toEdgeList l ++ toEdgeList r
toEdgeList (T v (T lv ll lr)  (T rv rl rr)) = toEdge v lv : toEdge v rv : ell ++ elr ++ erl ++ err
  where ell = toEdgeList ll
        elr = toEdgeList lr
        erl = toEdgeList rl
        err = toEdgeList rr