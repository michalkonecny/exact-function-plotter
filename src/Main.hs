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
import Miso.String (ms, fromMisoString)
import Miso.Svg as Svg


import qualified Data.CDAR as CDAR
-- import Data.CDAR (Dyadic)

import Function

-- example plotter input:
-- 10*x*(1-100/6*x*x*(1-100/20*x*x*(1 - 100/42*x*x*(1 - 100/72*x*x*(1-100/110*x*x)))))

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
  , _plotArea_numSegments :: Int
  }
  deriving (Show, Eq)

plotArea_extents :: Lens' PlotArea (Rectangle Rational)
plotArea_extents wrap (PlotArea a b) = fmap (\a' -> PlotArea a' b) (wrap a)
plotArea_numSegments :: Lens' PlotArea Int
plotArea_numSegments wrap (PlotArea a b) = fmap (\b' -> PlotArea a b') (wrap b)

data Rectangle a = Rectangle
  {
        _rect_left :: a
    ,   _rect_right :: a
    ,   _rect_down :: a
    ,   _rect_up :: a
  }
  deriving (Show, Eq)

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
  initialPlotArea = PlotArea (Rectangle (-1) 1 (-1) 1) initialSegCount
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

initialSegCount :: Int
initialSegCount = 50

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
    PlotArea (Rectangle xL xR yL yR) n = plotArea
    nQ = (toRational n) :: Rational
    xTolerance = (xR - xL)/nQ
    yTolerance = (yR - yL)/nQ
    xPrec, yPrec :: CDAR.Precision
    xPrec = round $ negate $ logBase 2 ((fromRational xTolerance) :: Double)
    yPrec = round $ negate $ logBase 2 ((fromRational yTolerance) :: Double)
    xPartition = [ ((nQ-i)*xL + i*xR)/nQ | i <- [0..nQ] ]
    segments = zip xPartition (tail xPartition)
    enclosure = catMaybes $ map encloseSegment segments
    encloseSegment (xiL, xiR) =
      let
        xiM = (xiL + xiR)/2
        yiM_A = evalRF (yPrec + 10) rf (CDAR.toApprox (xPrec + 10) xiM) 
        xi_A = (CDAR.toApprox (xPrec + 10) xiL) `CDAR.unionA` (CDAR.toApprox (xPrec + 10) xiR)
        (D (_yi_A : yid_A : _)) = evalRF (yPrec + 10) rf (xD (xPrec + 10) xi_A)
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

-- | Constructs a virtual DOM from a state
viewState :: State -> View Action
viewState s = div_ [] $ 
    [
      text "Function f(x) = " 
    , input_ [ size_ "80", onChange act_on_function ]
    , br_ []
    , text "Number of segments = " 
    , input_ [ size_ "5", value_ (ms $ show initialSegCount), onChange act_on_numSegments ]
    , br_ []
    ]
    ++ viewResult s
    -- ++ [text (ms $ show $ sum $ map (sum . map sumSegment) $ Map.elems $ s ^. state_fn_encls)]
    -- ++ [text $ ms $ show $ product [1..10000]]
    where
    -- sumSegment (PAPoint _ yLL yLR, PAPoint _ yRL yRR) =
    --   sum $ map fromRational [yLL,yLR,yRL,yRR] :: Double
    act_on_function fMS = 
      case (parseRF $ fromMisoString fMS) of
        Right rf -> NewFunction ("f", rf)
        Left errmsg -> NoOp -- TODO
    act_on_numSegments nMS = 
        case reads (fromMisoString nMS) of
            [(n,_)] -> NewPlotArea ((s ^. state_plotArea) & plotArea_numSegments .~ n)
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
    -- transformS = printf "translate(%f %f) scale(%f %f)" (-xLd) (-yLd) (w/(xRd-xLd)) (h/(yRd-yLd)) :: String
    -- transformS = printf "translate(%f %f)" (-xLd) (-yLd) :: String
    PlotArea (Rectangle xL xR yL yR) _ = _state_plotArea
    -- [xLd, xRd, yLd, yRd] = map fromRational [xL, xR, yL, yR] :: [Double]
    w = 800 :: Rational
    h = 800 :: Rational
    q2d :: Rational -> Double
    q2d = fromRational
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


