module MainLib
  ( run,
    Heap,
    MaxHeap,
    x,
  )
where

import qualified Data.Bifunctor
import Text.Printf

run :: String -> IO ()
run = putStrLn

x = [1, 2, 3, 4, 5] :: [Int]

y = fromList . concat . replicate 3 $ x :: MaxHeap Int

data Direction = L | R | O deriving (Show)

type Breadcrumbs = [Direction]

type Node a = (Breadcrumbs, a)

type Edge a = (Node a, Node a)

type DotNode = (String, String)

class Heap h where
  isEmpty :: (Ord a) => h a -> Bool
  insert :: (Ord a) => h a -> a -> h a
  height :: (Ord a) => h a -> Int
  fromList :: (Ord a) => [a] -> h a

data MaxHeap a = E | MaxHeap {node :: Node a, left :: MaxHeap a, right :: MaxHeap a} deriving (Show)

nid :: MaxHeap a -> Breadcrumbs
nid E = []
nid h = fst . node $ h

val :: MaxHeap a -> a
val = snd . node

maxHeapCreate :: Node a -> MaxHeap a -> MaxHeap a -> MaxHeap a
maxHeapCreate (nid, a) l r = MaxHeap {node = (nid, a), left = l, right = r}

instance Heap MaxHeap where
  isEmpty E = True
  isEmpty _ = False

  height E = 0
  height h = 1 + max (height . left $ h) (height . right $ h)

  insert h = indexedInsert h [O]

  fromList [] = E
  fromList as = foldl insert E as

indexedInsert :: (Ord a) => MaxHeap a -> Breadcrumbs -> a -> MaxHeap a
indexedInsert E breadcrumbs a = maxHeapCreate (breadcrumbs, a) E E
indexedInsert heap breadcrumbs a =
  if a > val heap
    then maxHeapCreate (node heap) (left heap) (indexedInsert (right heap) (nid heap ++ [R]) a)
    else maxHeapCreate (node heap) (indexedInsert (left heap) (nid heap ++ [L]) a) (right heap)

instance Functor MaxHeap where
  fmap _ E = E
  fmap f h = maxHeapCreate (nid h, f (val h)) (fmap f (left h)) (fmap f (right h))

heapEdges :: MaxHeap a -> [Edge a]
heapEdges E = []
heapEdges h = lEdge ++ rEdge ++ (heapEdges . left $ h) ++ (heapEdges . right $ h)
  where
    lEdge = case left h of
      E -> []
      _ -> [(node h, node . left $ h)]
    rEdge = case right h of
      E -> []
      _ -> [(node h, node . right $ h)]

heapNodes :: MaxHeap a -> [Node a]
heapNodes E = []
heapNodes h = node h : (heapNodes . left $ h) ++ (heapNodes . right $ h)

breadcrumbToString :: Breadcrumbs -> String
breadcrumbToString = foldr (\c acc -> show c ++ acc) ""

nodeCrumb = breadcrumbToString . fst

nodeToDot :: (Show a) => Node a -> DotNode
nodeToDot n = (nodeCrumb n, show . snd $ n)

dotNode :: DotNode -> String
dotNode n = printf "\n%s [ label=\"%s\" ]" crumb value where (crumb, value) = n

-- nodeToDotString = dotNode . nodeToDot

dotEdge :: (Show a) => Edge a -> String
dotEdge (n1, n2) = printf "\n%s -> %s" (nodeCrumb n1) (nodeCrumb n2)

heapDot :: (Show a) => MaxHeap a -> String
heapDot h = printf "digraph \n{\n %s \n %s \n }\n\n" nodes edges
  where
    nodes = concatMap (dotNode . nodeToDot) (heapNodes h)
    edges = concatMap dotEdge (heapEdges h)

toFile :: (Show a) => MaxHeap a -> IO ()
toFile E = return ()
toFile h = do
  writeFile "./graph.dot" contents
  where
    contents = heapDot h
