module Segment where

import Point (Point)
import qualified Point as P

class Segment s where
  first  :: (Point p, Ord a) => s (p a) -> p a
  second :: (Point p, Ord a) => s (p a) -> p a
  intersect :: (Point p, Ord a, Fractional a) => s (p a) -> s (p a) -> Maybe (p a)
  intersect s1 s2 = intersection (first s1) (second s1) (first s2) (second s2)
  fx :: (Point p, Ord a, Fractional a) => s (p a) -> a -> a
  fx = function

function :: (Segment s, Point p, Num a, Ord a, Fractional a) => s (p a) -> a -> a
function s x = let p1 = first s
                   p2 = second s
                   a = (P.y p2 - P.y p1) / (P.x p2 - P.x p1)
                   b = P.y p1 - a * P.x p1
                in a * x + b

intersection p1 p2 p3 p4 = 
  -- calculate  a1 b1 c1 where line joining point 1 and 2
  -- a1 x + b1 y + c1 = 0
  let a1 = P.y p2 - P.y p1
      b1 = P.x p1 - P.y p2
      c1 = P.x p2 * P.y p1 - P.x p1 * P.y p2
      -- r3 and r4
      r3 = a1 * P.x p3 + b1 * P.y p3 + c1
      r4 = a1 * P.x p4 + b1 * P.y p4 + c1
   in Nothing

