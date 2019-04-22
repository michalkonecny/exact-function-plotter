-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

-- | Haskell module declaration
module Main where

import Control.Concurrent
import Control.Concurrent.STM (TVar, atomically, retry, readTVar, newTVar, writeTVar)
import Control.Monad (void)

import Control.Monad.IO.Class (liftIO)

-- import Data.Time
import Text.Printf

import Control.Lens as Lens hiding (view)

import Language.Javascript.JSaddle (runJSaddle)

-- import Text.Printf

import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
-- import Data.Ratio ((%))

-- | Miso framework import
import qualified Miso
import Miso hiding (at)
import Miso.String (MisoString, ms, fromMisoString)
-- import Miso.Event.Types
import Miso.Svg as Svg
-- import Data.Aeson.Types

import qualified Data.CDAR as CDAR
-- import Data.CDAR (Dyadic)

import Expression
import Curve

type ItemName = String

data PlotItem_Type =
  PIT_Function | PIT_Curve -- | PIT_Fractal
  deriving (Show, Eq, Enum)

data PlotItem =
    PlotItem_Function RX
  | PlotItem_Curve ParamCurve2D
  -- | PlotItem_Fractal LIFS_Fractal
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
      _state_err :: Maybe String
    , _state_plotArea :: PlotArea
    , _state_items :: Map.Map ItemName PlotItem
    , _state_item_accuracies :: Map.Map ItemName PlotAccuracy
    , _state_item_workers :: Map.Map ItemName ThreadId
    , _state_item_encls :: Map.Map ItemName PAEnclosure
    -- , _state_plotArea_Movement :: PlotAreaMovement
  }
  deriving (Show, Eq)

-- makeLenses ''State

state_err :: Lens' State (Maybe String)
state_err wrap (State a b c d e f) = fmap (\a' -> State a' b c d e f) (wrap a)
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

type PASegment = (PAPoint Rational, PAPoint Rational)

data PAPoint a = 
  PAPoint 
  {
      _papt_xL :: a
    , _papt_xR :: a
    , _papt_yL :: a
    , _papt_yR :: a
  }
  deriving (Show, Eq)

-- makeLenses ''PAPoint
-- makeLenses ''Rectangle

data Action
  = NoOp
  | NoOpErr String
  | NewPlotArea !PlotArea
  | NewFunction !(ItemName, RX)
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
    (NewFunction (name, rf)) ->
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
      plotItem = PlotItem_Function rf
      plotAccuracy =
        case s ^. state_item_accuracies . at name of
          Just pac -> pac
          _ -> defaultPlotAccuracy
      s' =
        s & state_items . at name .~ Just plotItem 
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
    NoOpErr errMsg -> noEff $ s & state_err .~ (Just errMsg)
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
    (mxC, x) <-
      case maybePrevCompInfo of
        Just (oxC, _, _) | isPanned ->
          pure (get_xC_x oxC plotArea)
        _ -> 
          pure (Just xP, xP)
            where
            xP = plotArea_x plotArea
    -- start a new enclosure computation thread (if needed) and recurse:
    case mxC of
      Just xC ->
        do
        threadId <- forkIO $ sendNewEnclosureSegments isPanned plotArea plotAccuracy xC
        case isPanned of
          True -> waitForAreaAndAct (threadId : threadIds) (Just (x, plotArea, plotAccuracy))
          _    -> waitForAreaAndAct [threadId] (Just (x, plotArea, plotAccuracy))
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

  sendNewEnclosureSegments isPanned plotArea plotAccuracy dom =
    writeChan actionChan 
      (NewEnclosureSegments (name, shouldAppend, enclosure))
    where
    shouldAppend = isPanned
    enclosure = computeEnclosure plotItem plotArea plotAccuracy dom

computeEnclosure :: PlotItem -> PlotArea -> PlotAccuracy -> (Rational, Rational) -> PAEnclosure
computeEnclosure plotItem plotArea plotAccuracy (xCL, xCR) =
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
  enclosure = aseg xCL xCR
  (PlotItem_Function rf) = plotItem -- TODO
  aseg l r 
    | rd - ld > maxSegSize = asegDivision
    | rd - ld < 2 * minSegSize = 
        catMaybes [lrEnclosure0]
        -- catMaybes [lrEnclosureBest]
    | tol0 <= yTolerance = 
        catMaybes [lrEnclosure0]
    | tol1 <= yTolerance = 
        catMaybes [lrEnclosure1]
    | otherwise = asegDivision
    where
    ld = q2d l
    rd = q2d r
    asegDivision = aseg l m ++ aseg m r
      where m = (l+r)/2
    (lrEnclosure1, lrEnclosure0) = encloseSegment (l,r)
    tol0 = enclosure0Tolerance lrEnclosure0
    enclosure0Tolerance (Just (_, PAPoint _ _ yiL yiR)) = (q2d yiR) - (q2d yiL)
    enclosure0Tolerance _ = yWd
    tol1 = enclosure1Tolerance lrEnclosure1
    -- tol1Vert = enclosure1VertTolerance lrEnclosure1
    enclosure1Tolerance (Just (PAPoint xiL _ yiLL _yiLR, PAPoint xiR _ yiRL yiRR)) =
      xiW * yiW / (sqrt $ xiW^(2::Int) + yiD2^(2::Int))
      where
      yiW = (q2d yiRR) - (q2d yiRL)
      xiW = (q2d xiR) - (q2d xiL)
      yiD2 = yiD/2
      yiD
        | yiDavg >= 0 = yiDavg `min` (((max 0 yiLDd) `min` (max 0 yiRDd)) * xiW)
        | otherwise = (-yiDavg) `min` (((max 0 (-yiLDd)) `min` (max 0 (-yiRDd))) * xiW)
      yiDavg = (q2d yiRL) - (q2d yiLL)
      D (_ : yiLDd : _) = evalRX () rf (xD () ld)
      D (_ : yiRDd : _) = evalRX () rf (xD () rd)
    enclosure1Tolerance _ = yWd
  encloseSegment (xiL, xiR) =
    (enclosure1, enclosure0)
    where
    xiM = (xiL + xiR)/2
    yiM_A = evalRX (yPrec) rf (CDAR.toApprox (xPrec) xiM) 
    xi_A = (CDAR.toApprox (xPrec) xiL) `CDAR.unionA` (CDAR.toApprox xPrec xiR)
    (D (yi_A : yid_A : _)) = evalRX (yPrec) rf (xD xPrec xi_A)
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
          Just (PAPoint xiL xiL yiLL yiLR, PAPoint xiR xiR yiRL yiRR)
        _ -> Nothing
    enclosure0 =
      case (CDAR.lowerBound yi_A, CDAR.upperBound yi_A) of
        (CDAR.Finite yiL_D, CDAR.Finite yiR_D) ->
          let
            yiL = toRational yiL_D 
            yiR = toRational yiR_D 
          in
          Just (PAPoint xiL xiL yiL yiR, PAPoint xiR xiR yiL yiR)
        _ -> Nothing
  xPrec, yPrec :: CDAR.Precision
  xPrec = 10 + (round $ negate $ logBase 2 (minSegSize))
  yPrec = 10 + (round $ negate $ logBase 2 (yTolerance))

-- | Constructs a virtual DOM from a state
viewState :: State -> View Action
viewState s@State{..} = 
    div_ 
    [
      Miso.style_ (Map.singleton "font-size" "20pt")
    ] $ 
    viewFnControls "f1" s
    ++ viewFnControls "f2" s
    ++ viewResult s
    -- ++ [br_ [], text (ms $ show $ _state_plotArea), br_ []]
    -- ++ [br_ [], text (ms $ show $ _state_item_accuracies), br_ []]
    ++
    [
      text "Plot area: " 
    , input_ [ size_ "8", value_ (s2ms $ printf "%.4f" (q2d $ _rect_left _state_plotArea)), onChange act_on_xL ]
    , text " <= x <= " 
    , input_ [ size_ "8", value_ (s2ms $ printf "%.4f" (q2d $ _rect_right _state_plotArea)), onChange act_on_xR ]
    , text " , " 
    , input_ [ size_ "8", value_ (s2ms $ printf "%.4f" (q2d $ _rect_down _state_plotArea)), onChange act_on_yL ]
    , text " <= y <= " 
    , input_ [ size_ "8", value_ (s2ms $ printf "%.4f" (q2d $ _rect_up _state_plotArea)), onChange act_on_yR ]
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
    -- sumSegment (PAPoint _ yLL yLR, PAPoint _ yRL yRR) =
    --   sum $ map fromRational [yLL,yLR,yRL,yRR] :: Double

viewFnControls :: ItemName -> State -> [View Action]
viewFnControls fnname s@State{..} =
    [
      text $ s2ms $ printf "Function %s(x) = " fnname 
    , input_ [ size_ "80", onChange $ act_on_function]
    , br_ []
    , text $ s2ms $ printf "%s(x) accuracy ~ w/" fnname
    , input_ [ size_ "5", value_ (ms $ show $ _plotAccuracy_targetYSegments $ pac), onChange $ act_on_targetYsegs ]
    -- , br_ []
    , text "  " 
    , input_ [ size_ "5", value_ (ms $ show $ _plotAccuracy_minXSegments $ pac), onChange $ act_on_minXsegs ]
    , text " <= segments <= " 
    , input_ [ size_ "5", value_ (ms $ show $ _plotAccuracy_maxXSegments $ pac), onChange $ act_on_maxXsegs ]
    , br_ []
    , text (case _state_err of Nothing -> ""; Just msg -> (ms $ "Error: " ++ msg)) 
    , br_ []
    ]
    where
    pac = 
      case s ^. state_item_accuracies . at fnname of
        Just fpac -> fpac
        _ -> defaultPlotAccuracy
    act_on_function fMS = 
      case (parseRX $ fromMisoString fMS) of
        Right rf -> NewFunction (fnname, rf)
        Left _errmsg -> NoOp -- TODO
    act_on_targetYsegs = 
      act_on_plotAccuracy plotAccuracy_targetYsegments
    act_on_maxXsegs = 
      act_on_plotAccuracy plotAccuracy_maxXSegments
    act_on_minXsegs = 
      act_on_plotAccuracy plotAccuracy_minXSegments
    act_on_plotAccuracy paclens nMS = 
        case reads (fromMisoString nMS) of
            [(n,_)] -> NewAccuracy (fnname, fpac & paclens .~ n)
                where
                fpac = 
                  case (s ^. state_item_accuracies . at fnname) of
                    Just fpac2 -> fpac2
                    _ ->  defaultPlotAccuracy
            _ -> NoOp


h,w :: Integer
w = 800
h = 800

hQ, wQ :: Rational
hQ = toRational h
wQ = toRational w

viewResult :: State -> [View Action]
viewResult State {..} =
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
                ++ (concat $ map renderEnclosure $ Map.toList _state_item_encls)
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
    
    renderEnclosure (_fName, enclosure) =
      map renderSegment enclosure
      where
      renderSegment (PAPoint lxL lxR lyL lyR, PAPoint rxL rxR ryL ryR) =
        polygon_  [stroke_ "black", fill_ "pink", points_ pointsMS] []
        where
        pointsMS = ms $ intercalate " " $ map showPoint points
        showPoint (x,y) = showR x ++ "," ++ showR y
        showR :: Rational -> String
        showR q = show $ (fromRational q :: Double)
        points = 
          map transformPt (pointsL ++ pointsR) 
          where
          pointsL
            | lyL <= ryL = [(lxL,lyL), (lxR,lyL),(rxR, ryL)]
            | otherwise = [(lxL,lyL), (rxL,ryL),(rxR, ryL)]
          pointsR
            | lyR <= ryR = [(rxR,ryR), (rxL,ryR),(lxL, lyR)]
            | otherwise = [(rxR,ryR), (lxR,lyR),(lxL, lyR)]


q2d :: Rational -> Double
q2d = fromRational

d2q :: Double -> Rational
d2q = toRational

s2ms :: String -> MisoString
s2ms = ms