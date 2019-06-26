-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Haskell module declaration
module Rectangle where

import Control.Lens

-- import Control.Applicative ((<|>))
-- import Control.Monad       (liftM, liftM2)

-- import qualified Data.Map as Map

data Rectangle a = Rectangle
  {
        _rect_left :: a
    ,   _rect_right :: a
    ,   _rect_down :: a
    ,   _rect_up :: a
  }
  deriving (Show, Eq)

rect_left :: Lens' (Rectangle a) a
rect_left wrap (Rectangle a b c d) = fmap (\a' -> Rectangle a' b c d) (wrap a)
rect_right :: Lens' (Rectangle a) a
rect_right wrap (Rectangle a b c d) = fmap (\b' -> Rectangle a b' c d) (wrap b)
rect_down :: Lens' (Rectangle a) a
rect_down wrap (Rectangle a b c d) = fmap (\c' -> Rectangle a b c' d) (wrap c)
rect_up :: Lens' (Rectangle a) a
rect_up wrap (Rectangle a b c d) = fmap (\d' -> Rectangle a b c d') (wrap d)

rect_zoom :: (Fractional a) => a -> Rectangle a -> Rectangle a
rect_zoom ratio (Rectangle l r d u) = 
  Rectangle (xM - xri) (xM + xri) (yM - yri) (yM + yri)
  where
  xM = (l + r)/2
  yM = (d + u)/2
  xr = (r - l)/2
  yr = (u - d)/2
  xri = xr * ratio
  yri = yr * ratio

rect_move :: (Fractional a) => (a,a) -> Rectangle a -> Rectangle a
rect_move (xp,yp) (Rectangle l r d u) = 
  Rectangle (l + xd) (r + xd) (d + yd) (u + yd)
  where
  xd = xp * (r-l)
  yd = yp * (u-d)

rect_isPanned :: (Eq a, Num a) => Rectangle a -> Rectangle a -> Bool
rect_isPanned
  (Rectangle l1 r1 d1 u1)
  (Rectangle l2 r2 d2 u2) 
  =
  r1 - l1 == r2 - l2 && u1 - d1 == u2 - d2

rect_width :: (Num t, Ord t) => Rectangle t -> t
rect_width (Rectangle l r d u) = min (r-l) (u-d)

rect_hullAndWidthSq :: (Fractional t, Ord t) => Rectangle t -> Rectangle t -> ([(t, t)], t)
rect_hullAndWidthSq
  rect1@(Rectangle l1 r1 d1 u1)
  rect2@(Rectangle l2 r2 d2 u2)
    -- rect1 is inside rect2:
  | ld1_inside && rd1_inside && ru1_inside && lu1_inside = 
      ([ld2, rd2, ru2, lu2], sqr $ rect_width rect2)
    -- rect2 is inside rect1:
  | ld2_inside && rd2_inside && ru2_inside && lu2_inside = 
      ([ld1, rd1, ru1, lu1], sqr $ rect_width rect1)
    -- left edge of rect1 is inside the hull:
  | ld1_inside && lu1_inside = 
      ([ld2, rd2, rd1, ru1, ru2, lu2], sqr $ rect_width (Rectangle l2 r1 d2 u2))
    -- left edge of rect2 is inside the hull:
  | ld2_inside && lu2_inside = 
      ([ld1, rd1, rd2, ru2, ru1, lu1], sqr $ rect_width (Rectangle l1 r2 d1 u1))
    -- right edge of rect1 is inside the hull:
  | rd1_inside && ru1_inside = 
      ([ld2, rd2, ru2, lu2, ld1, lu1], sqr $ rect_width (Rectangle l1 r2 d2 u2))
    -- right edge of rect2 is inside the hull:
  | rd2_inside && ru2_inside = 
      ([ld1, rd1, ru1, lu1, ld2, lu2], sqr $ rect_width (Rectangle l2 r1 d1 u1))
    -- down edge of rect1 is inside the hull:
  | ld1_inside && rd1_inside = 
      ([ld2, rd2, ru2, ru1, lu1, lu2], sqr $ rect_width (Rectangle l2 r2 d2 u1))
    -- down edge of rect2 is inside the hull:
  | ld2_inside && rd2_inside = 
      ([ld1, rd1, ru1, ru2, lu2, lu1], sqr $ rect_width (Rectangle l1 r1 d1 u2))
    -- up edge of rect1 is inside the hull:
  | lu1_inside && ru1_inside = 
      ([ld2, ld1, rd1, rd2, ru2, lu2], sqr $ rect_width (Rectangle l2 r2 d1 u2))
    -- up edge of rect2 is inside the hull:
  | lu2_inside && ru2_inside = 
      ([ld1, ld2, rd2, rd1, ru1, lu1], sqr $ rect_width (Rectangle l1 r1 d2 u1))
    -- no edge fully inside the hull, rect2 located up right of rect1:
  | ru1_inside && ld2_inside = 
      ([ld1, rd1, rd2, ru2, lu2, lu1]
      , (rd1_dSq `min` lu1_dSq) `max` (rd2_dSq `min` lu2_dSq))
    -- no edge fully inside the hull, rect1 located up right of rect2:
  | ru2_inside && ld1_inside = 
      ([ld2, rd2, rd1, ru1, lu1, lu2]
      , (rd2_dSq `min` lu2_dSq) `max` (rd1_dSq `min` lu1_dSq))
    -- no edge fully inside the hull, rect2 located up left of rect1:
  | lu1_inside && rd2_inside = 
      ([ld1, rd1, ru1, ru2, lu2, ld2]
      , (ld1_dSq `min` ru1_dSq) `max` (ld2_dSq `min` ru2_dSq))
    -- no edge fully inside the hull, rect1 located up left of rect2:
  | lu2_inside && rd1_inside = 
      ([ld2, rd2, ru2, ru1, lu1, ld1]
      , (ld2_dSq `min` ru2_dSq) `max` (ld1_dSq `min` ru1_dSq))
    -- no corner is inside the hull, rect1 is taller and narrower than rect2:
  | l2 <= l1 =
    ([ld1, rd1, rd2, ru2, ru1, lu1, lu2, ld2], sqr $ rect_width (Rectangle l2 r2 d1 u1))
    -- no corner is inside the hull, rect2 is taller and narrower than rect1:
  | otherwise = 
    ([ld2, rd2, rd1, ru1, ru2, lu2, lu1, ld1], sqr $ rect_width (Rectangle l1 r1 d2 u2))
  where
  ld1 = (l1,d1)
  rd1 = (r1,d1)
  ru1 = (r1,u1)
  lu1 = (l1,u1)
  ld2 = (l2,d2)
  rd2 = (r2,d2)
  ru2 = (r2,u2)
  lu2 = (l2,u2)
  ld1_inside = l2 <= l1 && d2 <= d1
  ld2_inside = l1 <= l2 && d1 <= d2
  rd1_inside = r1 <= r2 && d2 <= d1
  rd2_inside = r2 <= r1 && d1 <= d2
  lu1_inside = l2 <= l1 && u1 <= u2
  lu2_inside = l1 <= l2 && u2 <= u1
  ru1_inside = r1 <= r2 && u1 <= u2
  ru2_inside = r2 <= r1 && u2 <= u1
  lu_dSq = line_distanceSq (lu1,lu2)
  rd1_dSq = lu_dSq rd1
  rd2_dSq = lu_dSq rd2
  rd_dSq = line_distanceSq (rd1,rd2)
  lu1_dSq = rd_dSq lu1
  lu2_dSq = rd_dSq lu2
  ru_dSq = line_distanceSq (ru1,ru2)
  ld1_dSq = ru_dSq ld1
  ld2_dSq = ru_dSq ld2
  ld_dSq = line_distanceSq (ld1,ld2)
  ru1_dSq = ld_dSq ru1
  ru2_dSq = ld_dSq ru2

line_distanceSq :: (Fractional t) => ((t,t),(t,t)) -> (t,t) -> t
line_distanceSq (pt1@(x1,y1),pt2@(x2,y2)) (x,y) =
  let d = pt_distanceSq pt1 pt2 in
  (sqr $ (x2-x1)*y + (y1-y2)*x + x1*y2 - x2*y1) / d

pt_distanceSq :: (Num t) => (t,t) -> (t,t) -> t
pt_distanceSq (x1,y1) (x2,y2) =
  (sqr $ x1-x2) + (sqr $ y1-y2)

sqr :: (Num t) => t -> t
sqr = (^(2::Int))
