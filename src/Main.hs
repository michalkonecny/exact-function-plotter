{-# OPTIONS_GHC -fno-warn-orphans  #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Haskell module declaration
module Main where

import Control.Concurrent
import Control.Concurrent.STM (TVar, atomically, retry, readTVar, newTVar, writeTVar)
import Control.Monad (void)

import Control.Monad.IO.Class (liftIO)

-- import Data.Time

import Control.Lens as Lens hiding (view)

import Language.Javascript.JSaddle (runJSaddle)

import Text.Printf

import Data.List (intercalate, find)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
-- import Data.Ratio ((%))

-- | Miso framework import
import qualified Miso
import Miso hiding (at)
import Miso.String (MisoString, ms, fromMisoString, ToMisoString(..))
-- import Miso.Event.Types
import Miso.Svg as Svg
-- import Data.Aeson.Types

import qualified Data.CDAR as CDAR
-- import Data.CDAR (Dyadic)

import Rectangle
import Expression
import Curve
import AffineFractal

type ItemName = String

data PlotItem_Type =
  PIT_Function | PIT_Curve | PIT_Fractal
  deriving (Show, Eq, Enum)

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
    , _state_item_encls :: Map.Map ItemName PAEnclosure
    -- , _state_plotArea_Movement :: PlotAreaMovement
  }
  deriving (Show, Eq)

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
state_item_encls :: Lens' State (Map.Map ItemName PAEnclosure)
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

type PAEnclosure = [PASegment]

type PASegment = [(Rational, Rational)] -- closed polyline

data Action
  = NoOp
  | SelectItem (Maybe ItemName)
  | NewPlotArea !PlotArea
  | NewPlotItem !(ItemName, PlotItem)
  | NewAccuracy !(ItemName, PlotAccuracy)
  | NewWorker !(ItemName, ThreadId)
  | NewEnclosureSegments !(ItemName, Bool, PAEnclosure)
  -- | SetDrag Bool
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = do
  -- pure ()
  actionChan <- newChan
  plotAreaTV <- atomically $ newTVar initialPlotArea
  plotAccuracyTV <- atomically $ newTVar Map.empty
  continueWithVars actionChan plotAreaTV plotAccuracyTV
  where
  initialPlotArea =
    Rectangle (-1) 1 (-1) 1
      -- initialTargetYSegments
      -- initialMaxXSegments
      -- initialMinXSegments
  -- initialPlotAreaMovement = 
  --   PlotAreaMovement False Nothing
  continueWithVars actionChan plotAreaTV plotAccuracyTV =
    runJSaddle undefined $ startApp App {..}
    where
    initialAction = NoOp
    model  = State Nothing initialPlotArea Map.empty Map.empty Map.empty Map.empty
    update = flip $ updateState actionChan plotAreaTV plotAccuracyTV
    view   = viewState
    events = defaultEvents
    subs   = [actionSub actionChan]
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')

initialTargetYSegments :: Int
initialTargetYSegments = 100
initialMaxXSegments :: Int
initialMaxXSegments = 1024
initialMinXSegments :: Int
initialMinXSegments = 8

actionSub :: Chan Action -> Sub Action
actionSub actionChan sink = void . liftIO . forkIO $ keepPassingActions
  where
  keepPassingActions =
    do
    action <- readChan actionChan
    sink action
    keepPassingActions

-- | Updates state, optionally introducing side effects
updateState :: (Chan Action) -> (TVar PlotArea) -> (TVar (Map.Map ItemName (TVar PlotAccuracy))) -> State -> Action -> Effect Action State
updateState actionChan plotAreaTV plotAccuracyTV s action =
  case action of
    (NewPlotArea pa) ->
      ((s & state_plotArea .~ pa) <#) $ liftIO $ do
        atomically $ writeTVar plotAreaTV pa
        return NoOp
    (NewAccuracy (name, pac)) ->
      ((s & state_item_accuracies . at name .~ Just pac) <#) $ liftIO $ do
        atomically $ do
          pacMap <- readTVar plotAccuracyTV
          case pacMap ^. at name of
            Just pacTV -> writeTVar pacTV pac
            _ -> pure ()
        return NoOp
    (NewPlotItem (name, plotItem)) ->
      (s' <#) $ liftIO $ do
        fnPlotAccuracyTV <- atomically $ do
          fnPlotAccuracyTV <- newTVar plotAccuracy
          paMap <- readTVar plotAccuracyTV
          writeTVar plotAccuracyTV $ paMap & (at name) .~ Just fnPlotAccuracyTV
          return fnPlotAccuracyTV
        case s ^. state_item_workers . at name of
          Just otid -> killThread otid -- stop previous worker thread
          _ -> pure ()
        -- start new worker thread:
        threadId <- forkIO $ enclWorker actionChan plotAreaTV fnPlotAccuracyTV name plotItem
        -- register the worker thread:
        pure $ NewWorker (name, threadId)
      where
      plotAccuracy =
        case s ^. state_item_accuracies . at name of
          Just pac -> pac
          _ -> defaultPlotAccuracy
      s' =
        s & state_items . at name .~ Just plotItem
          & state_selectedItem .~ Just name
          & state_item_accuracies . at name .~ Just plotAccuracy
          & state_item_workers . at name .~ Nothing
          & state_item_encls . at name .~ Nothing
    (NewWorker (name, tid)) ->
      noEff $ s & state_item_workers . at name .~ Just tid
    (NewEnclosureSegments (name, shouldAppend, encl)) ->
      noEff $
        s & state_item_encls . at name %~ addEncl
      where
      addEncl (Just oldEncl)
        | shouldAppend = Just $ oldEncl ++ encl
      addEncl _ = Just encl
    -- SetDrag isDrag ->
    --   if isDrag 
    --     then noEff s'
    --     else (s' <#) $ liftIO $ do
    --       atomically $ writeTVar plotAreaTV pa
    --       pure NoOp
    --   where
    --   s' = s & state_plotArea_Movement . plotAreaMovement_mouseDrag .~ isDrag
    --   pa = s ^. state_plotArea
    -- (Pan pos@(x,y)) ->
    --   noEff s2
    --   where
    --   isDrag = s ^. state_plotArea_Movement . plotAreaMovement_mouseDrag
    --   s1 = s 
    --     & state_plotArea_Movement . plotAreaMovement_mousePos .~ Just pos
    --   s2 
    --     | isDrag = s1 & state_plotArea . plotArea_extents %~ moveExtents
    --     | otherwise = s1
    --   moveExtents extents@(Rectangle xL xR yL yR) =
    --     case s ^. state_plotArea_Movement . plotAreaMovement_mousePos of
    --       Just (oldX, oldY) -> 
    --         let
    --           xRes = (round $ xR-xL) % w
    --           yRes = (round $ yR-yL) % h
    --           xd = (toRational $ x - oldX) * xRes
    --           yd = (toRational $ y - oldY) * yRes
    --         in
    --         Rectangle (xL - xd) (xR - xd) (yL + yd) (yR + yd)
    --       _ -> extents
    SelectItem maybeItemName -> noEff $ s & state_selectedItem .~ maybeItemName
    NoOp -> noEff s


enclWorker :: Chan Action -> TVar PlotArea -> TVar PlotAccuracy -> String -> PlotItem -> IO ()
enclWorker actionChan plotAreaTV fnPlotAccuracyTV name plotItem =
  waitForAreaAndAct [] Nothing
  where
  waitForAreaAndAct threadIds maybePrevCompInfo =
    do
    -- wait until there is a change in the plotArea, 
    -- then work out whether the change requires reset or append:
    (plotArea, plotAccuracy, isPanned) <- atomically $ do
      pa <- readTVar plotAreaTV
      pac <- readTVar fnPlotAccuracyTV
      case maybePrevCompInfo of
        Nothing -> pure (pa, pac, False)
        Just (_, oldpa, oldpac) ->
          if oldpa == pa && oldpac == pac then retry
          else
            pure (pa, pac, oldpac == pac && rect_isPanned oldpa pa)
    -- if resetting, kill any potentially active threads:
    case isPanned of
      False -> mapM_ killThread threadIds
      _ -> pure ()
    -- work over which interval to compute, if at all:
    (mdomC, dom) <-
      case (plotItem, maybePrevCompInfo) of
        (PlotItem_Curve (Curve2D dom _ _), _) ->
          pure $ if isPanned then (Nothing, dom) else (Just dom, dom)
        (PlotItem_Function _, Just (odomC, _, _)) | isPanned ->
          pure (get_xC_x odomC plotArea)
        _ ->
          pure (Just xP, xP)
            where
            xP = plotArea_x plotArea
    -- start a new enclosure computation thread (if needed) and recurse:
    case mdomC of
      Just domC ->
        do
        threadId <- forkIO $ sendNewEnclosureSegments isPanned plotArea plotAccuracy domC
        case isPanned of
          True -> waitForAreaAndAct (threadId : threadIds) (Just (dom, plotArea, plotAccuracy))
          _    -> waitForAreaAndAct [threadId] (Just (dom, plotArea, plotAccuracy))
      _ ->
        waitForAreaAndAct threadIds maybePrevCompInfo -- ie do nothing this time
    where
    get_xC_x (oxCL, oxCR) pa
      | xL < oxCL && oxCL <= xR && xR <= oxCR = (Just (xL, oxCL), (xL, oxCR))
          -- ie a part on the left needs computing
      | oxCL <= xL && xL <= oxCR && oxCR < xR = (Just (oxCR, xR), (oxCL, xR))
          -- ie a part on the right needs computing
      | oxCL <= xL && xR <= oxCR = (Nothing, (oxCL, oxCR))
      | otherwise = (Just (xL, xR), (xL, xR))
      where
      (Rectangle xL xR _ _) = pa
    plotArea_x pa = (xL, xR)
      where
      (Rectangle xL xR _ _) = pa
  
  isFunction =
    case plotItem of
      PlotItem_Function _ -> True
      _ -> False

  sendNewEnclosureSegments isPanned plotArea plotAccuracy dom =
    writeChan actionChan
      (NewEnclosureSegments (name, shouldAppend, enclosure))
    where
    shouldAppend = isFunction && isPanned
    enclosure =
      case plotItem of
        PlotItem_Fractal fr -> computeFractalEnclosure fr plotArea plotAccuracy
        _ -> computeEnclosure plotItem plotArea plotAccuracy dom

computeEnclosure :: PlotItem -> PlotArea -> PlotAccuracy -> (Rational, Rational) -> PAEnclosure
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
  enclosure = aseg tL tR
  aseg l r
    | rd - ld > maxSegSize = asegDivision
    | rd - ld < 2 * minSegSize =
        catMaybes [lrEnclosure0]
        -- catMaybes [lrEnclosureBest]
    | good0 =
        catMaybes [lrEnclosure0]
    | good1 =
        catMaybes [lrEnclosure1]
    | otherwise = asegDivision
    where
    ld = q2d l
    rd = q2d r
    asegDivision = aseg l m ++ aseg m r
      where m = (l+r)/2
    (lrEnclosure0, good0, lrEnclosure1, good1) =
      case plotItem of
        (PlotItem_Function rx) ->
          (hull0 e0, w0 <= yTolerance, fmap (uncurry hullTwoRects) e1, w1 <= yTolerance)
          where
          (e1, e0) = encloseSegment rx (l,r)
          w0 = enclosure0Width e0
          w1 = enclosure1Width rx e1
        (PlotItem_Curve (Curve2D _dom rx_x rx_y)) ->
          (hull0 e0, g0, fmap (uncurry hullTwoRects) e1, g1)
          where
          e0 = combine_exy e0x e0y
          e1 = combine_exy e1x e1y
          g0 = w0x <= xTolerance && w0y <= yTolerance
          g1 = w1x <= xTolerance && w1y <= yTolerance
          (e1x, e0x) = encloseSegment rx_x (l,r)
          (e1y, e0y) = encloseSegment rx_y (l,r)
          w0x = enclosure0Width e0x
          w0y = enclosure0Width e0y
          w1x = enclosure1Width rx_x e1x
          w1y = enclosure1Width rx_y e1y
          combine_exy
            (Just (Rectangle _ _ xiLL xiLR, Rectangle _ _ xiRL xiRR))
            (Just (Rectangle _ _ yiLL yiLR, Rectangle _ _ yiRL yiRR)) =
            Just (Rectangle xiLL xiLR yiLL yiLR, Rectangle xiRL xiRR yiRL yiRR)
          combine_exy _ _ = Nothing
        (PlotItem_Fractal _) -> error "computeEnclosure called for a fractal"
    hull0 (Just (Rectangle xiL _ _ _, Rectangle _ xiR yiL yiR)) = Just [(xiL, yiL), (xiR, yiL), (xiR, yiR), (xiL, yiR)]
    hull0 _ = Nothing
    enclosure0Width (Just (_, Rectangle _ _ yiL yiR)) = (q2d yiR) - (q2d yiL)
    enclosure0Width _ = yWd
    -- tol1Vert = enclosure1VertTolerance lrEnclosure1
    enclosure1Width rx (Just (Rectangle xiL _ yiLL _yiLR, Rectangle xiR _ yiRL yiRR)) =
      xiW * yiW / (sqrt $ xiW^(2::Int) + yiD2^(2::Int))
      where
      yiW = (q2d yiRR) - (q2d yiRL)
      xiW = (q2d xiR) - (q2d xiL)
      yiD2 = yiD/2
      yiD
        | yiDavg >= 0 = yiDavg `min` (((max 0 yiLDd) `min` (max 0 yiRDd)) * xiW)
        | otherwise = (-yiDavg) `min` (((max 0 (-yiLDd)) `min` (max 0 (-yiRDd))) * xiW)
      yiDavg = (q2d yiRL) - (q2d yiLL)
      D (_ : yiLDd : _) = evalRX () rx (xD () ld)
      D (_ : yiRDd : _) = evalRX () rx (xD () rd)
    enclosure1Width _ _ = yWd
  encloseSegment rx (xiL, xiR) =
    (enclosure1, enclosure0)
    where
    xiM = (xiL + xiR)/2
    yiM_A = evalRX (yPrec) rx (CDAR.toApprox (xPrec) xiM)
    xi_A = (CDAR.toApprox (xPrec) xiL) `CDAR.unionA` (CDAR.toApprox xPrec xiR)
    (D (yi_A : yid_A : _)) = evalRX (yPrec) rx (xD xPrec xi_A)
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
  xPrec, yPrec :: CDAR.Precision
  xPrec = 10 + (round $ negate $ logBase 2 (minSegSize))
  yPrec = 10 + (round $ negate $ logBase 2 (yTolerance))

computeFractalEnclosure :: AffineFractal -> PlotArea -> PlotAccuracy -> PAEnclosure
computeFractalEnclosure fractal plotArea plotAccuracy =
  enclosure0
  ++ (concat $ map (applyTransform enclosure0) $ concat transforms)
  ++ (concat $ map (applyTransform [boundsEncl]) lastLayerTransfroms)
  where
  AffineFractal curves transformations depth (Rectangle l r d u) = fractal
  boundsEncl = [(l,d), (r,d), (r,u), (l,u)]
  enclosure0 = 
    concat $ map encloseCurve curves
  encloseCurve curve =
    computeEnclosure (PlotItem_Curve curve) plotArea plotAccuracy (curve ^. curve2D_dom)
  transformationsToDepth n
    | n <= 1 = [transformations]
    | otherwise =
      [ t `aftCompose` tPrev | t <- transformations, tPrev <- prevLayer] : prevTransformations
    where
    prevTransformations@(prevLayer :_) = transformationsToDepth (n-1)
  (lastLayerTransfroms : transforms) = transformationsToDepth depth
  applyTransform encl (vx,vy,_) =
    map (map applyOnPt) encl
    where
    applyOnPt (x,y) = 
      (vx `v3prod` (x,y,1)
      ,vy `v3prod` (x,y,1))




---------------------------------------------------------------------------------
--- VIEW
---------------------------------------------------------------------------------

viewState :: State -> View Action
viewState s@State{..} =
    div_
    [
      Miso.style_ (Map.singleton "font-size" "20pt")
    ] $
    []
    ++ viewPlotAreaControls s
    ++ viewPlot s
    ++ viewAddItem s
    ++ viewItemList s
    ++ viewSelectedItemControls s
    -- ++ [br_ [], text (ms $ show $ _state_plotArea), br_ []]
    -- ++ [br_ [], text (ms $ show $ _state_item_accuracies), br_ []]

instance ToMisoString Rational where
  toMisoString q = s2ms $ printf "%.4f" (q2d q)
  fromMisoString _ = error "fromMisoString not defined for Rational"

viewAddItem :: State -> [View Action]
viewAddItem _s@State{..} =
  [
    text "Add: "
  , flip button_ [text "function"] [ onClick (NewPlotItem (freshName "f", (PlotItem_Function RXVarX)))]
  , flip button_ [text "sin(10x^2)"] [ onClick (NewPlotItem (freshName "sin(10x^2)", (PlotItem_Function (s2rx "sin(10*x^2)"))))]
  , flip button_ [text "x*sin(10/x)"] [ onClick (NewPlotItem (freshName "x*sin(10/x)", (PlotItem_Function (s2rx "x*sin(10/x)"))))]
  , text "; "
  , flip button_ [text "curve"] [ onClick (NewPlotItem (freshName "c", (PlotItem_Curve defaultCurve2D)))]
  , flip button_ [text "spiral"] [ onClick (NewPlotItem (freshName "spiral", (PlotItem_Curve spiral)))]
  , flip button_ [text "infty"] [ onClick (NewPlotItem (freshName "infty", (PlotItem_Curve infty)))]
  , flip button_ [text "mesh"] [ onClick (NewPlotItem (freshName "mesh", (PlotItem_Curve mesh)))]
  , text "; "
  , flip button_ [text "fractal"] [ onClick (NewPlotItem (freshName "fr", (PlotItem_Fractal defaultFractal)))]
  , br_ []
  ]
  where
  itemNames = Map.keys _state_items
  freshName prefix =
    case find (not . flip elem itemNames) $ prefix : [ prefix ++ show (i :: Int) | i <- [2..] ] of
      Just nm -> nm
      _ -> error "failed to find a default function name"
  spiral = Curve2D (0, 50) (s2rx "0.02*x*sin(x)") (s2rx "0.02*x*cos(x)")
  infty = Curve2D (0, 6.29) (s2rx "0.8*sin(x)") (s2rx "0.5*sin(2*x)")
  mesh = Curve2D (0, 6.29) (s2rx "0.8*sin(5*x)") (s2rx "0.5*sin(12*x)")
  

viewItemList :: State -> [View Action]
viewItemList _s@State{..} =
    [
      text "Items: "
    ]
    ++ map viewItemButton itemNames
    ++ [ br_ [] ]
    where
    itemNames = Map.keys _state_items
    viewItemButton itemName 
      | isSelected =
        flip button_ [text (ms itemName)] [ Miso.width_ "50",  activeColor, onClick (SelectItem Nothing) ]
      | otherwise =
        flip button_ [text (ms itemName)]  [ Miso.width_ "50", onClick (SelectItem (Just itemName)) ]
      where
      isSelected = (_state_selectedItem == Just itemName)
      activeColor = Miso.style_ $ Map.singleton "background-color" "pink"

viewSelectedItemControls :: State -> [View Action]
viewSelectedItemControls s@State{..} =
  case _state_selectedItem of
    Just itemName -> 
      case _state_items ^. at itemName of
        Just (PlotItem_Function _) -> viewFnControls itemName s
        Just (PlotItem_Curve _) -> viewCurveControls itemName s
        Just (PlotItem_Fractal _) -> viewFractalControls itemName s
        _ -> []
    _ -> []

viewFnControls :: ItemName -> State -> [View Action]
viewFnControls itemName s@State{..} =
    [
      text $ s2ms $ printf "Function %s(x) = " itemName
    , input_ [ size_ "80", value_ (ms $ showRX "x" rx), onChange $ act_on_function]
    , br_ []
    ]
    ++ viewPlotAccuracy itemName s
    where
    rx =
      case _state_items ^. at itemName of
        Just (PlotItem_Function rx2) -> rx2
        _ -> RXVarX
    act_on_function fMS =
      case (parseRX "x" $ fromMisoString fMS) of
        Right rx2 -> NewPlotItem (itemName, PlotItem_Function rx2)
        Left _errmsg -> NoOp -- TODO

viewEmbeddedCurveControls :: Curve2D -> (Curve2D -> Action) -> String -> State -> [View Action]
viewEmbeddedCurveControls curve mkAction curveName _s@State{..} =
    [
      text $ s2ms $ printf "Curve %s_x(t) = " curveName
    , input_ [ size_ "80", value_ (ms $ showRX "t" $ curve ^. curve2D_x), onChange $ act_on_x]
    , br_ []
    , text $ s2ms $ printf "Curve %s_y(t) = " curveName
    , input_ [ size_ "80", value_ (ms $ showRX "t" $ curve ^. curve2D_y), onChange $ act_on_y]
    , br_ []
    , input_ [ size_ "8", value_ (ms $ curve ^. curve2D_dom . _1), onChange $ act_on_t _1]
    , text " <= t <= "
    , input_ [ size_ "8", value_ (ms $ curve ^. curve2D_dom . _2), onChange $ act_on_t _2]
    , br_ []
    ]
    where
    act_on_x fMS =
      case (parseRX "t" $ fromMisoString fMS) of
        Right rx -> mkAction $ curve & curve2D_x .~ rx
        Left _errmsg -> NoOp -- TODO
    act_on_y fMS =
      case (parseRX "t" $ fromMisoString fMS) of
        Right rx -> mkAction $ curve & curve2D_y .~ rx
        Left _errmsg -> NoOp -- TODO
    act_on_t domlens tMS =
      case reads (fromMisoString tMS) of
        [(t,_)] -> mkAction $ curve & curve2D_dom . domlens .~ (d2q t)
        _ -> NoOp

viewCurveControls :: ItemName -> State -> [View Action]
viewCurveControls itemName s@State{..} =
    viewEmbeddedCurveControls curve mkAction itemName s
    ++ viewPlotAccuracy itemName s
    where
    mkAction c = NewPlotItem (itemName, PlotItem_Curve c)
    curve =
      case _state_items ^. at itemName of
        Just (PlotItem_Curve c) -> c
        _ -> defaultCurve2D

viewFractalControls :: ItemName -> State -> [View Action]
viewFractalControls itemName s@State{..} =
    [
      text $ s2ms $ printf "Fractal %s, curves:" itemName
    , br_ []
    ]
    ++ (concat $ map viewCurve $ zip [1..] curves) ++
    [
      text $ s2ms $ printf "Fractal %s, transforms:" itemName
    , br_ []
    ]
    -- ++ (concat $ map viewTransform $ zip [1..] transforms) -- TODO
    ++ viewPlotAccuracy itemName s
    where
    fractal =
      case _state_items ^. at itemName of
        Just (PlotItem_Fractal fr) -> fr
        _ -> defaultFractal
    AffineFractal curves transforms depth bounds = fractal
    viewCurve (i,curve) =
       viewEmbeddedCurveControls curve mkAction (itemName ++ "_curve" ++ show i) s
       where
       mkAction c = 
        NewPlotItem (itemName, PlotItem_Fractal $ fractal & affineFractal_curves . ix (i-1) .~ c)

viewPlotAccuracy :: ItemName -> State -> [View Action]
viewPlotAccuracy itemName s@State{..} =
    [
      text $ s2ms $ printf "%s accuracy ~ w/" itemName
    , input_ [ size_ "5", value_ (ms $ show $ _plotAccuracy_targetYSegments $ pac), onChange $ act_on_targetYsegs ]
    -- , br_ []
    , text "  "
    , input_ [ size_ "5", value_ (ms $ show $ _plotAccuracy_minXSegments $ pac), onChange $ act_on_minXsegs ]
    , text " <= segments <= "
    , input_ [ size_ "5", value_ (ms $ show $ _plotAccuracy_maxXSegments $ pac), onChange $ act_on_maxXsegs ]
    , br_ []
    ]
    where
    pac =
      case s ^. state_item_accuracies . at itemName of
        Just fpac -> fpac
        _ -> defaultPlotAccuracy
    act_on_targetYsegs =
      act_on_plotAccuracy plotAccuracy_targetYsegments
    act_on_maxXsegs =
      act_on_plotAccuracy plotAccuracy_maxXSegments
    act_on_minXsegs =
      act_on_plotAccuracy plotAccuracy_minXSegments
    act_on_plotAccuracy paclens nMS =
        case reads (fromMisoString nMS) of
            [(n,_)] -> NewAccuracy (itemName, fpac & paclens .~ n)
                where
                fpac =
                  case (s ^. state_item_accuracies . at itemName) of
                    Just fpac2 -> fpac2
                    _ ->  defaultPlotAccuracy
            _ -> NoOp


viewPlotAreaControls :: State -> [View Action]
viewPlotAreaControls s@State{..} =
    [
      text "Plot area: "
    , input_ [ size_ "8", value_ (ms $ _rect_left _state_plotArea), onChange act_on_xL ]
    , text " <= x <= "
    , input_ [ size_ "8", value_ (ms $ _rect_right _state_plotArea), onChange act_on_xR ]
    , text " , "
    , input_ [ size_ "8", value_ (ms $ _rect_down _state_plotArea), onChange act_on_yL ]
    , text " <= y <= "
    , input_ [ size_ "8", value_ (ms $ _rect_up _state_plotArea), onChange act_on_yR ]
    , br_ []
    , text "Zoom "
    , button_ [ onClick (zoomi (-1)) ] [ text "-"]
    , button_ [ onClick (zoomi 1) ] [text "+"]
    , text " Move "
    , button_ [ onClick (pani (1,0)) ] [ text "←"]
    , button_ [ onClick (pani (-1,0)) ] [ text "→"]
    , button_ [ onClick (pani (0,-1)) ] [ text "↑"]
    , button_ [ onClick (pani (0,1)) ] [ text "↓"]
    , br_ []
    ]
    where
    act_on_xL = act_on_plotArea rect_left
    act_on_xR = act_on_plotArea rect_right
    act_on_yL = act_on_plotArea rect_down
    act_on_yR = act_on_plotArea rect_up
    act_on_plotArea palens xMS =
        case reads (fromMisoString xMS) of
            [(x,_)] -> NewPlotArea ((s ^. state_plotArea) & palens .~ (d2q x))
            _ -> NoOp
    zoomi :: Int -> Action
    zoomi i =
      NewPlotArea $ rect_zoom ((110/100)^^(-i)) _state_plotArea
    pani :: (Rational, Rational) -> Action
    pani (xi,yi) =
      NewPlotArea $ rect_move ((1/10)*xi, (1/10)*yi) _state_plotArea
    -- sumSegment (Rectangle _ yLL yLR, Rectangle _ yRL yRR) =
    --   sum $ map fromRational [yLL,yLR,yRL,yRR] :: Double

h,w :: Integer
w = 800
h = 800

hQ, wQ :: Rational
hQ = toRational h
wQ = toRational w

viewPlot :: State -> [View Action]
viewPlot State {..} =
    [
        -- text (ms transformS),
        div_
          [
            Miso.style_ (Map.singleton "font-size" "12pt")
          --   Miso.style_ (Map.singleton "user-select" "none")
          -- , Miso.on "mousedown" emptyDecoder (const $ SetDrag True)
          -- -- , onNoDef "mousemove" xyPanDecoder id
          -- , Miso.on "mousemove" emptyDecoder (const $ NoOpErr "mousemove")
          -- , Miso.on "mouseup" emptyDecoder (const $ SetDrag False)
          -- , Miso.on "mouseout" emptyDecoder (const $ SetDrag False)
          -- , Miso.on "wheel" deltaYZoomDecoder id
          ]
          [
            svg_
              [ viewHeightAttr, viewWidthAttr
              ] $
                [rect_ [x_ "0", y_ "0", viewHeightAttr, viewWidthAttr, stroke_ "black", fill_ "none"] []]
                ++ (concat $ map renderEnclosure $ moveSelectedLast $ Map.toList _state_item_encls)
                ++ concat xGuides ++ concat yGuides
          ]
    ]
    where
    -- onNoDef = Miso.onWithOptions (Miso.Options True True) -- preventDefault = True
    -- xyPanDecoder :: Decoder Action
    -- xyPanDecoder = Decoder {..}
    --   where
    --     decodeAt = DecodeTarget mempty
    --     decoder = withObject "event" $ \o -> ms2action <$> (o .: "clientX") <*> (o .: "clientY")
    --     ms2action sX sY =
    --       case (reads (fromMisoString sX), reads (fromMisoString sY)) of
    --         ([(x,"")], [(y,"")]) -> Pan (x,y)
    --         _ -> NoOpErr $ printf ("sX = %s, sY = %s") (fromMisoString sX :: String) (fromMisoString sY :: String)
    -- deltaYZoomDecoder :: Decoder Action
    -- deltaYZoomDecoder = Decoder {..}
    --   where
    --     decodeAt = DecodeTarget mempty
    --     decoder = withObject "event" $ \o -> ms2action <$> (o .: "deltaY")
    --     ms2action s =
    --       case reads (fromMisoString s) of
    --         [(i,"")] -> Zoom (round (i :: Double))
    --         _ -> NoOp

    moveSelectedLast = aux Nothing
      where
      aux (Just sel) [] = [sel]
      aux _ [] = []
      aux msel (this@(itemName, _):rest)
        |  _state_selectedItem == Just itemName =
          aux (Just this) rest
        | otherwise =
          this : aux msel rest

    viewHeightAttr = Svg.height_ (ms (q2d hQ))
    viewWidthAttr = Svg.width_ (ms (q2d wQ))
    Rectangle xL xR yL yR = _state_plotArea
    -- [xLd, xRd, yLd, yRd] = map q2d [xL, xR, yL, yR]
    transformPt (x,y) = (transformX x, transformY y)
    transformX x = (x-xL)*wQ/(xR-xL)
    transformY y = hQ-(y-yL)*hQ/(yR-yL)
    xGuides =
      [ let xiMS = ms (q2d $ transformX xi) in
        [line_
         [x1_ xiMS, x2_ xiMS, y1_ "0", y2_ (ms (q2d hQ)),
          stroke_ "black", strokeDasharray_ "1 3"
         ] []
         ,
         text_ [x_ xiMS, y_ (ms (q2d hQ - 20))] [text (ms (q2d xi))]
        ]
      | xi <- xGuidePoints
      ]
      where
      xGuidePoints = [x1, x1+gran .. xR]
      gran = 10.0 ^^ (round $ logBase 10 (q2d $ (xR - xL)/10) :: Int)
      x1 = gran * (fromInteger $ ceiling (xL / gran)) :: Rational
    yGuides =
      [ let yiMS = ms (q2d $ transformY yi) in
        [line_
         [y1_ yiMS, y2_ yiMS, x1_ "0", x2_ (ms (q2d wQ)),
          stroke_ "black", strokeDasharray_ "1 3"
         ] []
         ,
         text_ [y_ yiMS, x_ (ms (q2d wQ - 30))] [text (ms (q2d yi))]
        ]
      | yi <- yGuidePoints
      ]
      where
      yGuidePoints = [y1, y1+gran .. yR]
      gran = 10.0 ^^ (round $ logBase 10 (q2d $ (yR - yL)/10) :: Int)
      y1 = gran * (fromInteger $ ceiling (yL / gran)) :: Rational

    renderEnclosure (itemName, enclosure) =
      map renderSegment enclosure
      where
      renderSegment pointsPre =
        polygon_  (points_ pointsMS : style) []
        where
        style =
          case _state_selectedItem of
            Just selectedName | selectedName == itemName -> 
              [stroke_ "black", fill_ "#ffc0cb", fillOpacity_ "0.7"]
            _ -> 
              [stroke_ "#707070", fill_ "#ffc0cb", fillOpacity_ "0.4"]
        pointsMS = ms $ intercalate " " $ map showPoint points
        showPoint (x,y) = showR x ++ "," ++ showR y
        showR :: Rational -> String
        showR q = show $ (fromRational q :: Double)
        points = map transformPt pointsPre

q2d :: Rational -> Double
q2d = fromRational

d2q :: Double -> Rational
d2q = toRational

s2ms :: String -> MisoString
s2ms = ms