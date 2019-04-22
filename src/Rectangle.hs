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

hullTwoRects :: Ord t => Rectangle t -> Rectangle t -> [(t, t)]
hullTwoRects
  _rect1@(Rectangle l1 r1 d1 u1)
  _rect2@(Rectangle l2 r2 d2 u2)
    -- rect1 is inside rect2:
  | ld1_inside && rd1_inside && ru1_inside && lu1_inside = [ld2, rd2, ru2, lu2]
    -- rect2 is inside rect1:
  | ld2_inside && rd2_inside && ru2_inside && lu2_inside = [ld1, rd1, ru1, lu1]
    -- left edge of rect1 is inside the hull:
  | ld1_inside && lu1_inside = [ld2, rd2, rd1, ru1, ru2, lu2]
    -- left edge of rect2 is inside the hull:
  | ld2_inside && lu2_inside = [ld1, rd1, rd2, ru2, ru1, lu1]
    -- right edge of rect1 is inside the hull:
  | rd1_inside && ru1_inside = [ld2, rd2, ru2, lu2, ld1, lu1]
    -- right edge of rect2 is inside the hull:
  | rd2_inside && ru2_inside = [ld1, rd1, ru1, lu1, ld2, lu2]
    -- down edge of rect1 is inside the hull:
  | ld1_inside && rd1_inside = [ld2, rd2, ru2, ru1, lu1, lu2]
    -- down edge of rect2 is inside the hull:
  | ld2_inside && rd2_inside = [ld1, rd1, ru1, ru2, lu2, lu1]
    -- up edge of rect1 is inside the hull:
  | lu1_inside && ru1_inside = [ld2, ld1, rd1, rd2, ru2, lu2]
    -- up edge of rect2 is inside the hull:
  | lu2_inside && ru2_inside = [ld1, ld2, rd2, rd1, ru1, lu1]
    -- no edge fully inside the hull, rect2 located up right of rect1:
  | ru1_inside && ld2_inside = [ld1, rd1, rd2, ru2, lu2, lu1]
    -- no edge fully inside the hull, rect1 located up right of rect2:
  | ru2_inside && ld1_inside = [ld2, rd2, rd1, ru1, lu1, lu2]
    -- no edge fully inside the hull, rect2 located up left of rect1:
  | lu1_inside && rd2_inside = [ld1, rd1, ru1, ru2, lu2, ld2]
    -- no edge fully inside the hull, rect1 located up left of rect2:
  | lu2_inside && rd1_inside = [ld2, rd2, ru2, ru1, lu1, ld1]
    -- no corner is inside the hull, rect1 is taller and narrower than rect2:
  | l2 <= l1 =
    [ld1, rd1, rd2, ru2, ru1, lu1, lu2, ld2]
    -- no corner is inside the hull, rect2 is taller and narrower than rect1:
  | otherwise = 
    [ld2, rd2, rd1, ru1, ru2, lu2, lu1, ld1]
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
