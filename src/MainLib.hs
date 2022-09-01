module MainLib
  ( run,
    Heap,
    MaxHeap,
  )
where

run :: String -> IO ()
run = putStrLn

class Heap h where
  isEmpty :: (Ord a) => h a -> Bool
  insert :: (Ord a) => a -> h a -> h a

  --   merge :: (Ord a) => h a -> h a -> h a
  height :: (Ord a) => h a -> Int

data MaxHeap a
  = Empty
  | T Int a (MaxHeap a) (MaxHeap a)

instance Heap MaxHeap where
  isEmpty Empty = True
  isEmpty _ = False

  height Empty = 0
  height (T _ _ l r) = 1 + max (height l) (height r)

  insert a Empty = T 0 a Empty Empty
  insert a (T x a' l r) =
    if a > a'
      then T x a' (insert a l) r
      else T x a' l (insert a r)

instance (Show a) => Show (MaxHeap a) where
    show Empty = ""
    show (T v a r l) = "\n " ++ show a ++ show r ++ show l


-- identList :: MaxHeap -> [Int]
-- identList Empty = []
-- identList (T i v l r) = 