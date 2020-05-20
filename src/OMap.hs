{-# LANGUAGE MultiParamTypeClasses #-}
module OMap where

import qualified Data.FingerTree as FT
import Data.FingerTree (FingerTree, (<|), (|>), (><), ViewR(..), ViewL(..), Measured(..))
import Data.Foldable
import Data.Semigroup

data Entry k v = Entry k v
  deriving (Show)

instance Functor (Entry k) where
  fmap f (Entry k v) = Entry k (f v)

instance Foldable (Entry k) where
  foldMap f (Entry _ v) = f v

data Prio k v = NoPrio | Prio k v

instance Ord k => Semigroup (Prio k v) where
  x <> NoPrio = x
  NoPrio <> y = y
  x@(Prio kx _) <> y@(Prio ky _)
    | kx < ky = y
    | otherwise = x  

instance Ord k => Monoid (Prio k v) where
  mempty = NoPrio
  mappend x y = x <> y

instance Ord k => Measured (Prio k v) (Entry k v) where
  measure (Entry k v) = Prio k v

newtype OMap k v = OMap (FingerTree (Prio k v) (Entry k v))
  deriving Show

empty :: Ord k => OMap k v
empty = OMap FT.empty

insert :: Ord k => k -> v -> OMap k v -> OMap k v
insert k v (OMap t) = OMap $ (l |> Entry k v) >< r
  where
    (l, r) = FT.split below t
    below NoPrio = False
    below (Prio p _) = p > k

remove :: Ord k => k -> OMap k v -> OMap k v
remove x (OMap t) = OMap $ foldl' (\t' e@(Entry k v) -> if x == k then t' else t' |> e) FT.empty t

fromList :: Ord k => [(k, v)] -> OMap k v
fromList = foldr (uncurry insert) empty


maxBelow :: Ord k => k -> OMap k v -> Maybe (k, v)
maxBelow k (OMap t)
  | FT.null l = Nothing
  | otherwise = Just (case FT.viewr l of
    _ :> (Entry k v) -> (k, v))
  where
    (l, r) = FT.split below t
    below NoPrio = False
    below (Prio p _) = p >= k

minAbove :: Ord k => k -> OMap k v -> Maybe (k ,v)
minAbove k (OMap t)
  | FT.null r = Nothing
  | otherwise = Just (case FT.viewl r of
    (Entry k v) :< _ -> (k, v))
    where
      (l, r) = FT.split above t
      above NoPrio = False
      above (Prio p _) = p > k

