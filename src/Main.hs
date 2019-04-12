{-# LANGUAGE CPP #-}
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

-- | Miso framework import
import qualified Miso
import Miso hiding (at)
import Miso.String (MisoString, ms, fromMisoString)
import Miso.Svg as Svg


import qualified Data.CDAR as CDAR
-- import Data.CDAR (Dyadic)

import Function

{-
    A function is represented symbolically and rendered via a piece-wise affine enclosure.
    The direction of each segment is determined by an enclosure of the derivative
    over the whole segment.

    We cache the enclosure and recompute it asynchronously after each pan/zoom.
-}
data State
  = State 
  {
      _state_plotArea :: PlotArea
    , _state_fn_exprs :: Map.Map String RF
    , _state_fn_workers :: Map.Map String ThreadId
    , _state_fn_encls :: Map.Map String PAEnclosure
  }
  deriving (Show, Eq)

-- makeLenses ''State

state_plotArea :: Lens' State PlotArea
state_plotArea wrap (State a b c d) = fmap (\a' -> State a' b c d) (wrap a)
state_fn_exprs :: Lens' State (Map.Map String RF)
state_fn_exprs wrap (State a b c d) = fmap (\b' -> State a b' c d) (wrap b)
state_fn_workers :: Lens' State (Map.Map String ThreadId)
state_fn_workers wrap (State a b c d) = fmap (\c' -> State a b c' d) (wrap c)
state_fn_encls :: Lens' State (Map.Map String PAEnclosure)
state_fn_encls wrap (State a b c d) = fmap (\d' -> State a b c d') (wrap d)

data PlotArea = 
  PlotArea
  {
    _plotArea_extents :: Rectangle Rational
  , _plotArea_targetYSegments :: Int
  , _plotArea_maxXSegments :: Int
  , _plotArea_minXSegments :: Int
  }
  deriving (Show, Eq)

plotArea_extents :: Lens' PlotArea (Rectangle Rational)
plotArea_extents wrap (PlotArea a b c d) = fmap (\a' -> PlotArea a' b c d) (wrap a)
plotArea_targetYsegments :: Lens' PlotArea Int
plotArea_targetYsegments wrap (PlotArea a b c d) = fmap (\b' -> PlotArea a b' c d) (wrap b)
plotArea_maxXSegments :: Lens' PlotArea Int
plotArea_maxXSegments wrap (PlotArea a b c d) = fmap (\c' -> PlotArea a b c' d) (wrap c)
plotArea_minXSegments :: Lens' PlotArea Int
plotArea_minXSegments wrap (PlotArea a b c d) = fmap (\d' -> PlotArea a b c d') (wrap d)

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

type PAEnclosure = [PASegment]

type PASegment = (PAPoint Rational, PAPoint Rational)

data PAPoint a = 
  PAPoint 
  {
      _papt_x :: a
    , _papt_yL :: a
    , _papt_yR :: a
  }
  deriving (Show, Eq)

-- makeLenses ''PAPoint
-- makeLenses ''Rectangle

data Action
  = NoOp
  | NewPlotArea !PlotArea
  | NewFunction !(String, RF)
  | NewWorker !(String, ThreadId)
  | NewEnclosure !(String, PAEnclosure)
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = do
  -- pure ()
  actionChan <- newChan
  plotAreaTV <- atomically $ newTVar initialPlotArea
  continueWithVars actionChan plotAreaTV
  where
  initialPlotArea = 
    PlotArea (Rectangle (-1) 1 (-1) 1) 
      initialTargetYSegments
      initialMaxXSegments
      initialMinXSegments
  continueWithVars actionChan plotAreaTV =
    runJSaddle undefined $ startApp App {..}
    where
    initialAction = NoOp
    model  = State initialPlotArea Map.empty Map.empty Map.empty
    update = updateState actionChan plotAreaTV
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
updateState :: (Chan Action) -> (TVar PlotArea) -> Action -> State -> Effect Action State
updateState _ plotAreaTV (NewPlotArea pa) s =
  ((s & state_plotArea .~ pa) <#) $
    liftIO $ do
      atomically $ writeTVar plotAreaTV pa
      pure NoOp
updateState actionChan plotAreaTV (NewFunction (name, rf)) s =
  ((s & state_fn_exprs . at name .~ Just rf) <#) $
      liftIO $ do
        printf "NewFunction %s\n" name
        threadId <- forkIO $ enclWorker actionChan plotAreaTV name rf
        pure $ NewWorker (name, threadId) 
updateState _ _ (NewWorker (name, tid)) s =
  ((s & state_fn_workers . at name .~ Just tid) <# ) $
    liftIO $ do
      printf "NewWorker %s %s" name (show tid) -- MAYBEFIX this message is not showing in Firefox console ..
      pure NoOp
updateState _ _ (NewEnclosure (name, encl)) s =
  noEff $ s & state_fn_encls . at name .~ Just encl
updateState _ _ _ s = noEff s

enclWorker :: (Chan Action) -> (TVar PlotArea) -> String -> RF -> IO ()
enclWorker actionChan plotAreaTV name rf =
  waitForAreaAndAct Nothing
  where
  waitForAreaAndAct maybePrevThreadArea =
    do
    (maybePrevThreadId, plotArea) <- atomically $ do
      pa <- readTVar plotAreaTV
      case maybePrevThreadArea of
        Nothing -> pure (Nothing, pa)
        Just (threadId, oldpa) ->
          if oldpa == pa then retry
          else pure (Just threadId, pa)
    case maybePrevThreadId of
      Nothing -> pure ()
      Just tid -> killThread tid
    threadId <- forkIO $ sendNewEnclosure plotArea
    waitForAreaAndAct $ Just (threadId, plotArea)
  sendNewEnclosure plotArea =
    do
    writeChan actionChan (NewEnclosure (name, enclosure))
    where
    PlotArea (Rectangle xL xR yL yR) yN xMaxN xMinN = plotArea
    yNQ = (toRational yN) :: Rational
    xMinNQ = (toRational xMinN) :: Rational
    xMaxNQ = (toRational xMaxN) :: Rational
    maxSegSize = (xR - xL)/xMinNQ
    minSegSize = (xR - xL)/xMaxNQ
    yTolerance = (yR - yL)/yNQ
    enclosure = aseg xL xR
    aseg l r 
      | r - l > maxSegSize = asegDivision
      | r - l < 2 * minSegSize = catMaybes [lrEnclosure]
      | enclosureGood lrEnclosure = catMaybes [lrEnclosure]
      | otherwise = asegDivision
      where
      asegDivision = aseg l m ++ aseg m r
        where m = (l+r)/2
      lrEnclosure = encloseSegment (l,r)
      enclosureGood (Just (PAPoint xiL yiLL yiLR, PAPoint xiR yiRL yiRR)) =
        (yiRW <= yTolerance || (yiW > 0) && (xiW*yiRW/yiW) <= yTolerance)
        && 
        (yiLW <= yTolerance || (yiW > 0) && (xiW*yiLW/yiW) <= yTolerance)
        where
        yiLW = yiLR - yiLL
        yiRW = yiRR - yiRL
        xiW = xiR - xiL
        yiW = min (abs $ yiRL - yiLL) (abs $ yiRR - yiLL)
      enclosureGood _ = False
    -- xPartition = [ ((nQ-i)*xL + i*xR)/nQ | i <- [0..nQ] ]
    -- segments = zip xPartition (tail xPartition)
    -- enclosure = catMaybes $ map encloseSegment segments
    encloseSegment (xiL, xiR) =
      let
        xiM = (xiL + xiR)/2
        yiM_A = evalRF (yPrec) rf (CDAR.toApprox (xPrec) xiM) 
        xi_A = (CDAR.toApprox (xPrec) xiL) `CDAR.unionA` (CDAR.toApprox xPrec xiR)
        (D (_yi_A : yid_A : _)) = evalRF (yPrec) rf (xD xPrec xi_A)
      in
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
          Just (PAPoint xiL yiLL yiLR, PAPoint xiR yiRL yiRR)
        _ -> Nothing
    xPrec, yPrec :: CDAR.Precision
    xPrec = 10 + (round $ negate $ logBase 2 (q2d minSegSize))
    yPrec = 10 + (round $ negate $ logBase 2 (q2d yTolerance))

-- | Constructs a virtual DOM from a state
viewState :: State -> View Action
viewState s@State{..} = div_ [] $ 
    [
      text "Function f(x) = " 
    , input_ [ size_ "80", onChange act_on_function ]
    , br_ []
    , text "Enclosure width tolerance = " 
    , input_ [ size_ "5", value_ (ms $ show _plotArea_targetYSegments), onChange act_on_targetYsegs ]
    , br_ []
    , text "Maximum segments = " 
    , input_ [ size_ "5", value_ (ms $ show _plotArea_maxXSegments), onChange act_on_maxXsegs ]
    , br_ []
    , text "Minimum segments = " 
    , input_ [ size_ "5", value_ (ms $ show _plotArea_minXSegments), onChange act_on_minXsegs ]
    , br_ []
    , text "Plot area: " 
    , input_ [ size_ "8", value_ (s2ms $ printf "%.4f" (q2d $ _rect_left _plotArea_extents)), onChange act_on_xL ]
    , text " <= x <= " 
    , input_ [ size_ "8", value_ (s2ms $ printf "%.4f" (q2d $ _rect_right _plotArea_extents)), onChange act_on_xR ]
    , input_ [ size_ "8", value_ (s2ms $ printf "%.4f" (q2d $ _rect_down _plotArea_extents)), onChange act_on_yL ]
    , text " <= y <= " 
    , input_ [ size_ "8", value_ (s2ms $ printf "%.4f" (q2d $ _rect_up _plotArea_extents)), onChange act_on_yR ]
    , br_ []
    ]
    ++ viewResult s
    -- ++ [text (ms $ show $ sum $ map (sum . map sumSegment) $ Map.elems $ s ^. state_fn_encls)]
    -- ++ [text $ ms $ show $ product [1..10000]]
    where
    PlotArea{..} = _state_plotArea
    -- sumSegment (PAPoint _ yLL yLR, PAPoint _ yRL yRR) =
    --   sum $ map fromRational [yLL,yLR,yRL,yRR] :: Double
    act_on_function fMS = 
      case (parseRF $ fromMisoString fMS) of
        Right rf -> NewFunction ("f", rf)
        Left _errmsg -> NoOp -- TODO
    act_on_targetYsegs nMS = 
        case reads (fromMisoString nMS) of
            [(n,_)] -> NewPlotArea ((s ^. state_plotArea) & plotArea_targetYsegments .~ n)
            _ -> NoOp
    act_on_maxXsegs nMS = 
        case reads (fromMisoString nMS) of
            [(n,_)] -> NewPlotArea ((s ^. state_plotArea) & plotArea_maxXSegments .~ n)
            _ -> NoOp
    act_on_minXsegs nMS = 
        case reads (fromMisoString nMS) of
            [(n,_)] -> NewPlotArea ((s ^. state_plotArea) & plotArea_minXSegments .~ n)
            _ -> NoOp
    act_on_xL xMS = 
        case reads (fromMisoString xMS) of
            [(xL,_)] -> NewPlotArea ((s ^. state_plotArea) & plotArea_extents . rect_left .~ (d2q xL))
            _ -> NoOp
    act_on_xR xMS = 
        case reads (fromMisoString xMS) of
            [(xR,_)] -> NewPlotArea ((s ^. state_plotArea) & plotArea_extents . rect_right .~ (d2q xR))
            _ -> NoOp
    act_on_yL yMS = 
        case reads (fromMisoString yMS) of
            [(yL,_)] -> NewPlotArea ((s ^. state_plotArea) & plotArea_extents . rect_down .~ (d2q yL))
            _ -> NoOp
    act_on_yR yMS = 
        case reads (fromMisoString yMS) of
            [(yR,_)] -> NewPlotArea ((s ^. state_plotArea) & plotArea_extents . rect_up .~ (d2q yR))
            _ -> NoOp

viewResult :: State -> [View action]
viewResult State {..} =
    [
        -- text (ms transformS),
        svg_ [ viewHeightAttr, viewWidthAttr ] $
          -- pure $ g_ [ transform_ (ms transformS)] $
            [rect_ [x_ "0", y_ "0", viewHeightAttr, viewWidthAttr, stroke_ "black", fill_ "none"] []]
            ++ (concat $ map renderEnclosure $ Map.toList _state_fn_encls)
            ++ concat xGuides ++ concat yGuides
    ]
    where
    viewHeightAttr = Svg.height_ (ms (q2d h))
    viewWidthAttr = Svg.width_ (ms (q2d w))
    PlotArea (Rectangle xL xR yL yR) _ _ _ = _state_plotArea
    -- [xLd, xRd, yLd, yRd] = map q2d [xL, xR, yL, yR]
    w = 800 :: Rational
    h = 800 :: Rational
    transformPt (x,y) = (transformX x, transformY y)
    transformX x = (x-xL)*w/(xR-xL)
    transformY y = h-(y-yL)*h/(yR-yL)
    xGuides = 
      [ let xiMS = ms (q2d $ transformX xi) in
        [line_ 
         [x1_ xiMS, x2_ xiMS, y1_ "0", y2_ (ms (q2d h)), 
          stroke_ "black", strokeDasharray_ "1 3"
         ] []
         ,
         text_ [x_ xiMS, y_ (ms (q2d h - 20))] [text (ms (q2d xi))]
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
         [y1_ yiMS, y2_ yiMS, x1_ "0", x2_ (ms (q2d w)), 
          stroke_ "black", strokeDasharray_ "1 3"
         ] []
         ,
         text_ [y_ yiMS, x_ (ms (q2d w - 30))] [text (ms (q2d yi))]
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
      renderSegment (PAPoint lx lyL lyR, PAPoint rx ryL ryR) =
        polygon_  [stroke_ "black", fill_ "pink", points_ pointsMS] []
        where
        pointsMS = ms $ intercalate " " $ map showPoint points
        showPoint (x,y) = showR x ++ "," ++ showR y
        showR :: Rational -> String
        showR q = show $ (fromRational q :: Double)
        points = map transformPt [(lx, lyL), (lx, lyR), (rx, ryR), (rx, ryL)]


q2d :: Rational -> Double
q2d = fromRational

d2q :: Double -> Rational
d2q = toRational

s2ms :: String -> MisoString
s2ms = ms