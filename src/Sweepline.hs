{-# LANGUAGE ScopedTypeVariables #-}
module Sweepline where

import Segment (Segment)
import Point (Point)
import qualified Segment as Seg
import qualified Point as P
import qualified Data.PriorityQueue.FingerTree as PQ
import qualified OMap as SL
import Data.Maybe

data Event s p a = Start (p a) (s (p a)) | End (p a) (s (p a)) | Intersection (p a) (s (p a)) (s (p a)) 
    deriving(Ord, Eq, Show)

event2prio :: (Segment s, Point p, Ord a) => Event s p a -> (a, Event s p a)
event2prio s@(Start        p   _) = (P.x p ,s)
event2prio s@(End          p   _) = (P.x p, s)
event2prio s@(Intersection p _ _) = (P.x p, s)

emptySL :: (Segment s, Point p, Ord a) => SL.OMap a (s (p a))
emptySL = SL.empty 

sweepline :: (Segment s, Point p, Ord a, Fractional a) => [s (p a)] -> [(p a, s (p a), s (p a))]
sweepline segments =
  let events = segments >>= \seg -> [Start (Seg.first seg) seg, End (Seg.second seg) seg]
      eq = PQ.fromList $ event2prio <$> events
   in go eq SL.empty []

go :: (Segment s, Point p, Ord a, Fractional a)
   => PQ.PQueue a (Event s p a)  -- Event queue
   -> SL.OMap a (s (p a))        -- Sweep line
   -> [(p a, s (p a), s (p a))]  -- interseciton accumulator
   -> [(p a, s (p a), s (p a))]
go eq sl acc =
  case PQ.minView eq of
    Just (Start p segE, eq') -> 
      let segEy = Seg.fx segE (P.x p)
          mSegA = do
            (_, segA) <- SL.maxBelow segEy sl
            ipa <- Seg.intersect segE segA
            return (P.x ipa, Intersection ipa segE segA)
          mSegB = do
            (_, segB) <- SL.minAbove segEy sl
            ipb <- Seg.intersect segE segB
            return (P.x ipb, Intersection ipb segE segB)
          events = catMaybes [mSegA, mSegB]
          eq'' = foldl (\queue (k,v) -> PQ.insert k v queue) eq' events
          sl' = SL.insert segEy segE sl
       in go eq'' sl' acc
    Just (End p segE, eq') ->
      let segEy = Seg.fx segE (P.x p)
          fp = Seg.first segE
          ev  = do
            (_, segA) <- SL.maxBelow segEy sl
            (_, segB) <- SL.minAbove segEy sl
            ip <- Seg.intersect segA segB
            return (P.x ip, Intersection ip segA segB)
          sl' = SL.remove segEy sl
          eq'' = foldl (\queue (k,v) -> PQ.insert k v queue) eq' ev
       in go eq'' sl' acc
    Just (Intersection p segE1 segE2, eq') ->
     go eq' sl $ (p, segE1, segE2) : acc
    Nothing -> acc

