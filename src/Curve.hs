-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Haskell module declaration
module Curve where

import Control.Lens

-- import Control.Applicative ((<|>))
-- import Control.Monad       (liftM, liftM2)

-- import qualified Data.Map as Map


-- import Data.CDAR

import Expression

data Curve2D =
  Curve2D
  {
    _curve2D_dom :: (Rational, Rational)
  , _curve2D_x :: RX
  , _curve2D_y :: RX
  }
  deriving (Show, Eq)

defaultCurve2D :: Curve2D
defaultCurve2D = Curve2D (0,1) RXVarX RXVarX

curve2D_dom :: Lens' Curve2D (Rational, Rational)
curve2D_dom wrap (Curve2D a b c) = fmap (\a' -> Curve2D a' b c) (wrap a)
curve2D_x :: Lens' Curve2D RX
curve2D_x wrap (Curve2D a b c) = fmap (\b' -> Curve2D a b' c) (wrap b)
curve2D_y :: Lens' Curve2D RX
curve2D_y wrap (Curve2D a b c) = fmap (\c' -> Curve2D a b c') (wrap c)
