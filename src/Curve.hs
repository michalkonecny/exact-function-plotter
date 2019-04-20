-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Haskell module declaration
module Curve where

-- import Control.Lens

-- import Control.Applicative ((<|>))
-- import Control.Monad       (liftM, liftM2)

-- import qualified Data.Map as Map


-- import Data.CDAR

import Expression

data ParamCurve2D =
  Curve2D
  {
    _curve2D_dom :: (Rational, Rational)
  , _curve2D_x :: RX
  , _curve2D_y :: RX
  }

