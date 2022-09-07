module BinTree where

-- parent :: (Integral a) => a -> a
parent :: (RealFrac a, Integral b) => a -> b
parent i = floor ((i -1) / 2)

left :: Num a => a -> a
left i = 2 * i + 1

right :: Num a => a -> a
right i = 2 * (i + 1)
