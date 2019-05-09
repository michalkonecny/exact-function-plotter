{-# OPTIONS_GHC -fno-warn-orphans  #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Haskell module declaration
module Main.Enclosure where

import Control.Lens as Lens hiding (view)

-- import Text.Printf

-- import Data.List (intercalate, find)
-- import qualified Data.Map as Map
import Data.Maybe (catMaybes)
-- import Data.Ratio ((%))

import qualified Data.CDAR as CDAR
-- import Data.CDAR (Dyadic)

import Rectangle
import Expression
import Curve
import AffineFractal
import Main.Model

computeEnclosure :: PlotItem -> PlotArea -> PlotAccuracy -> (Rational, Rational) -> (PAEnclosure Rational)
computeEnclosure plotItem plotArea plotAccuracy (tL, tR) =
  enclosure
  where
  Rectangle xL xR yL yR = plotArea
  PlotAccuracy yN xMaxN xMinN = plotAccuracy
  xLd = q2d xL
  xRd = q2d xR
  xWd = xRd - xLd
  yLd = q2d yL
  yRd = q2d yR
  yWd = yRd - yLd
  yNd = (fromIntegral yN) :: Double
  xMinNd = (fromIntegral xMinN) :: Double
  xMaxNd = (fromIntegral xMaxN) :: Double
  maxSegSize = xWd/xMinNd
  minSegSize = xWd/xMaxNd
  yTolerance = yWd/yNd
  xTolerance = xWd/yNd
  tolerance = yTolerance `max` xTolerance
  initPrec :: CDAR.Precision
  initPrec = 10 + (round $ negate $ logBase 2 (min xTolerance yTolerance))
  enclosure = aseg initPrec tL tR
  aseg prec l r
    | rd - ld > maxSegSize = asegDivision prec
    | rd - ld < 2 * minSegSize =
        catMaybes [lrEnclosure0]
        -- catMaybes [lrEnclosureBest]
    | good0 =
        catMaybes [lrEnclosure0]
    | good1 =
        catMaybes [lrEnclosure1]
    | otherwise = asegDivision precNext
    where
    ld = q2d l
    rd = q2d r
    asegDivision p = aseg p l m ++ aseg p m r
      where m = (l+r)/2
    enclosuresPrecs@(enclosuresPrec : _) = 
      map withPrec precs
      where
      withPrec p = encloseSegmentItem p (xTolerance, yTolerance) yWd plotItem (l,r)
    precs = iterate (\p -> (3*p `div` 2) + 10) prec
    (lrEnclosure0, width0, good0, lrEnclosure1, width1, good1) = enclosuresPrec
    precNext = 
      pickPrecStopImproving width0 width1 $ zip precs enclosuresPrecs
      where
      pickPrecStopImproving prevW0 prevW1 ((p,(_,w0,_,_,w1,_)) : rest)
        | w0 <= prevW0 - tolerance/10 = pickPrecStopImproving w0 w1 rest
        | w1 <= prevW1 - tolerance/10 = pickPrecStopImproving w0 w1 rest
        | otherwise = p
      pickPrecStopImproving _ _ _ = error "pickPrecStopImproving: infinite list ended"

encloseSegmentItem :: 
  CDAR.Precision
  -> (Double, Double)
  -> Double
  -> PlotItem
  -> (Rational, Rational)
  -> (Maybe (PASegment Rational), Double, Bool
    , Maybe (PASegment Rational), Double, Bool)
encloseSegmentItem p (xTolerance, yTolerance) yWd plotItem (l,r) =
  case plotItem of
    (PlotItem_Function rx) ->
      (hull0 <$> e0
      , w0
      , w0 <= yTolerance
      , fmap (\(r1,r2) -> (fst $ rect_hullAndWidthSq r1 r2, Just w1)) e1
      , w1
      , w1 <= yTolerance)
      where
      (e0, e1) = encloseSegmentRX p rx (l,r)
      w0 = enclosure0Width e0
      w1 = enclosure1Width e1
      hull0 (Rectangle xiL _ _ _, Rectangle _ xiR yiL yiR) =
        ([(xiL, yiL), (xiR, yiL), (xiR, yiR), (xiL, yiR)], Just w0)
      enclosure0Width (Just (_, Rectangle _ _ yiL yiR)) = (q2d yiR) - (q2d yiL)
      enclosure0Width _ = yWd
      -- tol1Vert = enclosure1VertTolerance lrEnclosure1
      enclosure1Width (Just (Rectangle xiL _ yiLL _yiLR, Rectangle xiR _ yiRL yiRR)) =
        xiW * yiW / (sqrt $ xiW^(2::Int) + yiD2^(2::Int))
        where
        yiW = (q2d yiRR) - (q2d yiRL)
        xiW = (q2d xiR) - (q2d xiL)
        yiD2 = yiD/2
        yiD
          | yiDavg >= 0 = yiDavg `min` (((max 0 yiLDd) `min` (max 0 yiRDd)) * xiW)
          | otherwise = (-yiDavg) `min` (((max 0 (-yiLDd)) `min` (max 0 (-yiRDd))) * xiW)
        yiDavg = (q2d yiRL) - (q2d yiLL)
        D (_ : yiLDd : _) = evalRX () rx (xD () (q2d l))
        D (_ : yiRDd : _) = evalRX () rx (xD () (q2d r))
      enclosure1Width _ = yWd

    (PlotItem_Curve (Curve2D _dom rx_x rx_y)) ->
      (hull0 <$> e0
      , min w0x w0y
      , w0x <= xTolerance || w0y <= yTolerance
      , e1Hull
      , w1
      , w1 <= xTolerance `max` yTolerance)
      where
      (e0x, e1x) = encloseSegmentRX p rx_x (l,r)
      (e0y, e1y) = encloseSegmentRX p rx_y (l,r)
      e0 = combine_exy <$> e0x <*> e0y
      e1 = combine_exy <$> e1x <*> e1y
      (w0x, w0y) = enclosure0Width e0
      w0 = min w0x w0y
      (e1Hull, w1) = 
        case e1 of
          Just (r1,r2) ->
            let (e1H, w1Sq) = rect_hullAndWidthSq r1 r2 in
              (Just (e1H, Just w1), sqrt $ q2d w1Sq)
          _ -> (Nothing, yWd)
      hull0 (_, Rectangle xiL xiR yiL yiR) =
        ([(xiL, yiL), (xiR, yiL), (xiR, yiR), (xiL, yiR)], Just w0)
      combine_exy
        (Rectangle _ _ xiLL xiLR, Rectangle _ _ xiRL xiRR)
        (Rectangle _ _ yiLL yiLR, Rectangle _ _ yiRL yiRR)
        =
        (Rectangle xiLL xiLR yiLL yiLR, Rectangle xiRL xiRR yiRL yiRR)
      enclosure0Width
        (Just (_, Rectangle xiRL xiRR yiRL yiRR))
        = (q2d xiRR - q2d xiRL, q2d yiRR - q2d yiRL)
      enclosure0Width _ = (yWd, yWd)

    (PlotItem_Fractal _) -> error "encloseSegmentItem called for a fractal"

encloseSegmentRX ::
  CDAR.Precision ->
  RX {-^ expression defining function @f@ -} ->
  (Rational, Rational) {-^ interval over which to evaluate @f@ -} ->
  (Maybe (Rectangle Rational, Rectangle Rational),
   Maybe (Rectangle Rational, Rectangle Rational))
encloseSegmentRX p rx (xiL, xiR) =
  (enclosure0, enclosure1)
  where
  xiM = (xiL + xiR)/2
  yiM_A = evalRX p rx (CDAR.toApprox p xiM)
  xi_A = (CDAR.toApprox p xiL) `CDAR.unionA` (CDAR.toApprox p xiR)
  (D (yi_A : yid_A : _)) = evalRX p rx (xD p xi_A)
  enclosure1 =
    case (CDAR.lowerBound yiM_A, CDAR.upperBound yiM_A, CDAR.lowerBound yid_A, CDAR.upperBound yid_A) of
      (CDAR.Finite yiML_D, CDAR.Finite  yiMR_D, CDAR.Finite  yidL_D, CDAR.Finite  yidR_D) ->
        let
          yiML = toRational yiML_D
          yiMR = toRational yiMR_D
          yidL = toRational yidL_D
          yidR = toRational yidR_D
          rad = (xiR - xiL)/2
          yiLL = yiML - rad*yidR
          yiLR = yiMR - rad*yidL
          yiRL = yiML + rad*yidL
          yiRR = yiMR + rad*yidR
        in
        Just (Rectangle xiL xiL yiLL yiLR, Rectangle xiR xiR yiRL yiRR)
      _ -> Nothing
  enclosure0 =
    case (CDAR.lowerBound yi_A, CDAR.upperBound yi_A) of
      (CDAR.Finite yiL_D, CDAR.Finite yiR_D) ->
        let
          yiL = toRational yiL_D
          yiR = toRational yiR_D
        in
        Just (Rectangle xiL xiL yiL yiR, Rectangle xiR xiR yiL yiR)
      _ -> Nothing

computeFractalEnclosure :: AffineFractal -> PlotArea -> PlotAccuracy -> (PAEnclosure Rational)
computeFractalEnclosure fractal plotArea plotAccuracy =
  enclosure0
  ++ (concat $ map (applyTransform enclosure0) $ concat $ reverse transforms)
  ++ (concat $ map (applyTransform [boundsEncl]) lastLayerTransforms)
  where
  AffineFractal curves transformations depth (Rectangle l r d u) = fractal
  boundsEncl = ([(l,d), (r,d), (r,u), (l,u)], Nothing)
  enclosure0 =
    concat $ map encloseCurve curves
  encloseCurve curve =
    computeEnclosure (PlotItem_Curve curve) plotArea plotAccuracy (curve ^. curve2D_dom)
  transformationsToDepth n
    | n <= 0 = [[aftIdentity]]
    | otherwise =
      [ t `aftCompose` tPrev | t <- transformations, tPrev <- prevLayer] : prevTransformations
    where
    prevTransformations@(prevLayer :_) = transformationsToDepth (n-1)
  (lastLayerTransforms : transforms) = transformationsToDepth depth
  applyTransform encl (vx,vy,_) =
    map applyOnSeg encl
    where
    applyOnSeg (seg, _mwidth) =
      (map applyOnPt seg, Nothing)
    applyOnPt (x,y) =
      (vx `v3prod` (x,y,1)
      ,vy `v3prod` (x,y,1))



