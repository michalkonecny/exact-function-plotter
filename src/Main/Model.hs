{-# OPTIONS_GHC -fno-warn-orphans  #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Haskell module declaration
module Main.Model where

import Control.Concurrent

import Control.Lens as Lens hiding (view)

-- import Text.Printf

-- import Data.List (intercalate, find)
import qualified Data.Map as Map
-- import Data.Maybe (catMaybes)
-- import Data.Ratio ((%))

import Rectangle
import Expression
import Curve
import AffineFractal

type ItemName = String

data PlotItem =
    PlotItem_Function RX
  | PlotItem_Curve Curve2D
  | PlotItem_Fractal AffineFractal
  deriving (Show, Eq)

{-
    A function is represented symbolically and rendered via a piece-wise affine enclosure.
    The direction of each segment is determined by an enclosure of the derivative
    over the whole segment.

    We cache the enclosure and recompute it asynchronously after each pan/zoom.
-}
data State
  = State
  {
      _state_selectedItem :: Maybe ItemName
    , _state_plotArea :: PlotArea
    , _state_items :: Map.Map ItemName PlotItem
    , _state_item_accuracies :: Map.Map ItemName PlotAccuracy
    , _state_item_workers :: Map.Map ItemName ThreadId
    , _state_item_encls :: Map.Map ItemName (Scaling, PAEnclosure Double, [RootEnclosure Double])
        -- the two rationals are x,y scaling factors, respectively
        -- the coordinates in the enclosure have been multiplied by these factors before converting to Double
        -- most of the time these factors agree with the plotting scale, allowing the coordinates to be used
        -- for plotting after a translation

    -- , _state_plotArea_Movement :: PlotAreaMovement
  }
  deriving (Show, Eq)

type Scaling = (Rational, Rational)

-- makeLenses ''State

state_selectedItem :: Lens' State (Maybe String)
state_selectedItem wrap (State a b c d e f) = fmap (\a' -> State a' b c d e f) (wrap a)
state_plotArea :: Lens' State PlotArea
state_plotArea wrap (State a b c d e f) = fmap (\b' -> State a b' c d e f) (wrap b)
state_items :: Lens' State (Map.Map ItemName PlotItem)
state_items wrap (State a b c d e f) = fmap (\c' -> State a b c' d e f) (wrap c)
state_item_accuracies :: Lens' State (Map.Map ItemName PlotAccuracy)
state_item_accuracies wrap (State a b c d e f) = fmap (\d' -> State a b c d' e f) (wrap d)
state_item_workers :: Lens' State (Map.Map ItemName ThreadId)
state_item_workers wrap (State a b c d e f) = fmap (\e' -> State a b c d e' f) (wrap e)
state_item_encls :: Lens' State (Map.Map ItemName (Scaling, PAEnclosure Double, [RootEnclosure Double]))
state_item_encls wrap (State a b c d e f) = fmap (\f' -> State a b c d e f') (wrap f)
-- state_plotArea_Movement :: Lens' State PlotAreaMovement
-- state_plotArea_Movement wrap (State a b c d e f) = fmap (\f' -> State a b c d e f') (wrap f)

data PlotAccuracy =
  PlotAccuracy
  {
    _plotAccuracy_targetYSegments :: Int
  , _plotAccuracy_maxXSegments :: Int
  , _plotAccuracy_minXSegments :: Int
  }
  deriving (Show, Eq)

defaultPlotAccuracy :: PlotAccuracy
defaultPlotAccuracy = PlotAccuracy 100 1024 8

plotAccuracy_targetYsegments :: Lens' PlotAccuracy Int
plotAccuracy_targetYsegments wrap (PlotAccuracy a b c) = fmap (\a' -> PlotAccuracy a' b c) (wrap a)
plotAccuracy_maxXSegments :: Lens' PlotAccuracy Int
plotAccuracy_maxXSegments wrap (PlotAccuracy a b c) = fmap (\b' -> PlotAccuracy a b' c) (wrap b)
plotAccuracy_minXSegments :: Lens' PlotAccuracy Int
plotAccuracy_minXSegments wrap (PlotAccuracy a b c) = fmap (\c' -> PlotAccuracy a b c') (wrap c)

type PlotArea = Rectangle Rational

-- data PlotAreaMovement =
--   PlotAreaMovement
--   {
--     _plotAreaMovement_mouseDrag :: Bool
--   , _plotAreaMovement_mousePos :: Maybe (Int, Int)
--   }
--   deriving (Show, Eq)

-- plotAreaMovement_mouseDrag :: Lens' PlotAreaMovement Bool
-- plotAreaMovement_mouseDrag wrap (PlotAreaMovement a b) = fmap (\a' -> PlotAreaMovement a' b) (wrap a)
-- plotAreaMovement_mousePos :: Lens' PlotAreaMovement (Maybe (Int, Int))
-- plotAreaMovement_mousePos wrap (PlotAreaMovement a b) = fmap (\b' -> PlotAreaMovement a b') (wrap b)

type PAEnclosure t = [PASegment t]

type PASegment t = ([(t, t)], Maybe Double) -- closed polyline

type RootEnclosure t = ((t,t), (Maybe Int, Maybe Int)) -- location of some number of roots

data Action
  = NoOp
  | SelectItem (Maybe ItemName)
  | NewPlotArea !PlotArea
  | NewPlotItem !(ItemName, PlotItem)
  | NewAccuracy !(ItemName, PlotAccuracy)
  | NewWorker !(ItemName, ThreadId)
  | NewEnclosureSegments !(ItemName, Bool, (Rational, Rational), (PAEnclosure Double, [RootEnclosure Double]))
  -- | SetDrag Bool
  deriving (Show, Eq)

q2d :: Rational -> Double
q2d = fromRational

d2q :: Double -> Rational
d2q = toRational

splitIntoBatches :: Int -> [a] -> [[a]]
splitIntoBatches n list =
  case rest of
    [] -> [batch]
    _ -> batch : splitIntoBatches n rest
  where
  (batch, rest) = splitAt n list
