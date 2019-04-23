-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Haskell module declaration
module AffineFractal where

import Control.Lens

-- import Control.Applicative ((<|>))
-- import Control.Monad       (liftM, liftM2)

-- import qualified Data.Map as Map


import Data.CDAR

-- import Expression
import Curve

data AffineFractal =
  AffineFractal
  {
    _affineFractal_curves :: [Curve2D]
  , _affineFractal_transforms :: [AffineTransform Rational] 
  }
  deriving (Show, Eq)

affineFractal_curves :: Lens' AffineFractal [Curve2D]
affineFractal_curves wrap (AffineFractal a b) = fmap (\a' -> AffineFractal a' b) (wrap a)
affineFractal_transforms :: Lens' AffineFractal [AffineTransform Rational]
affineFractal_transforms wrap (AffineFractal a b) = fmap (\b' -> AffineFractal a b') (wrap b)

defaultFractal :: AffineFractal
defaultFractal =
  AffineFractal [defaultCurve2D] [((0,0.5,0),(0.5,0,0),(0,0,1))]

class CanAffineTransform t where
  addAT :: t -> t -> t
  mulAT :: t -> t -> t

instance CanAffineTransform Rational where
  addAT = (+)
  mulAT = (*)

instance CanAffineTransform Approx where
  addAT = (+)
  mulAT = (*)

type Vector3 t = (t, t, t)

v3map :: (a -> b) -> Vector3 a -> Vector3 b
v3map f (a0,a1,a2) = (f a0, f a1, f a2)

v3prod :: (CanAffineTransform t) => Vector3 t -> Vector3 t -> t
v3prod (a0,a1,a2) (b0,b1,b2) = (a0*:b0) +: (a1*:b1) +: (a2*:b2)
  where
  (*:) = mulAT
  (+:) = addAT

type AffineTransform t = (Vector3 t, Vector3 t, Vector3 t)
   -- 3 x 3 matrix, row-major

atMap :: (a -> b) -> AffineTransform a -> AffineTransform b
atMap f (v0,v1,v2) = (fv v0, fv v1, fv v2)
  where
  fv = v3map f

atTranspose :: AffineTransform t -> AffineTransform t
atTranspose 
  ((a00,a01,a02),
   (a10,a11,a12),
   (a20,a21,a22))
  = 
  ((a00,a10,a20),
   (a01,a11,a21),
   (a02,a12,a22))

atCompose :: (CanAffineTransform t) => AffineTransform t -> AffineTransform t -> AffineTransform t
atCompose _at1@(v10, v11, v12) at2 = 
  ((v10 *. v20, v10 *. v21, v10 *. v22), 
   (v11 *. v20, v11 *. v21, v11 *. v22),
   (v12 *. v20, v12 *. v21, v12 *. v22))
  where
  (*.) = v3prod
  (v20, v21, v22) = atTranspose at2

