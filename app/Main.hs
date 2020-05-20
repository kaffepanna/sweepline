module Main where
import Segment as S
import Point as P
import Sweepline


data Loc a = Loc a a
  deriving Show
data Seg l = Seg l l
  deriving Show

instance Point Loc  where
  x (Loc a _) = a
  y (Loc _ b) = b

instance Segment Seg where
  first (Seg a b) = if P.x a <= P.x b then a else b 
  second (Seg a b) = if P.x a > P.x b then a else b


segs = [Seg (Loc 1.0 2.0) (Loc (-1.0) 2.0)]

main :: IO ()
main = do
  intersections <- pure $ sweepline segs
  print intersections

