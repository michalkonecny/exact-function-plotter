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
rect_zoom ratio (Rectangle xL xR yL yR) = 
  Rectangle (xM - xri) (xM + xri) (yM - yri) (yM + yri)
  where
  xM = (xL + xR)/2
  yM = (yL + yR)/2
  xr = (xR - xL)/2
  yr = (yR - yL)/2
  xri = xr * ratio
  yri = yr * ratio

rect_move :: (Fractional a) => (a,a) -> Rectangle a -> Rectangle a
rect_move (xp,yp) (Rectangle xL xR yL yR) = 
  Rectangle (xL + xd) (xR + xd) (yL + yd) (yR + yd)
  where
  xd = xp * (xR-xL)
  yd = yp * (yR-yL)

rect_isPanned :: (Eq a, Num a) => Rectangle a -> Rectangle a -> Bool
rect_isPanned
  (Rectangle l1 r1 d1 u1)
  (Rectangle l2 r2 d2 u2) 
  =
  r1 - l1 == r2 - l2 && u1 - d1 == u2 - d2

-- hullTwoRects :: Rectangle Rational -> Rectangle Rational -> [(Rational, Rational)]
-- hullTwoRects (Rectangle xL1 xR1 yL1 yR1) (Rectangle xL2 xR2 yL2 yR2) =
--   undefined
--   where

