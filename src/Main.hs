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

import Control.Lens as Lens hiding (view)

import Language.Javascript.JSaddle (runJSaddle)

import Text.Printf

import Data.List (intercalate, find)
import qualified Data.Map as Map
-- import Data.Maybe (catMaybes)
-- import Data.Ratio ((%))

-- | Miso framework import
import qualified Miso
import Miso hiding (at)
import Miso.String (MisoString, ms, fromMisoString, ToMisoString(..))
-- import Miso.Event.Types
import Miso.Svg as Svg
-- import Data.Aeson.Types

-- import qualified Data.CDAR as CDAR
-- import Data.CDAR (Dyadic)

import Rectangle
import Expression
import Curve
import AffineFractal
import Main.Model
import Main.Enclosure

-- | Entry point for a miso application
main :: IO ()
main = do
  -- pure ()
  actionChan <- newChan
  plotAreaTV <- atomically $ newTVar initialPlotArea
  itemMapTV <- atomically $ newTVar Map.empty
  continueWithVars actionChan plotAreaTV itemMapTV
  where
  initialPlotArea =
    Rectangle (-1) 1 (-1) 1
  continueWithVars actionChan plotAreaTV itemMapTV =
    runJSaddle undefined $ startApp App {..}
    where
    initialAction = NoOp
    model  = State Nothing initialPlotArea Map.empty Map.empty Map.empty Map.empty
    update = flip $ updateState actionChan plotAreaTV itemMapTV
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
updateState ::
  (Chan Action) ->
  (TVar PlotArea) ->
  (TVar (Map.Map ItemName (TVar (PlotItem, PlotAccuracy)))) ->
  State -> Action -> Effect Action State
updateState actionChan plotAreaTV itemMapTV s action =
  case action of
    (NewPlotArea pa) ->
      ((s & state_plotArea .~ pa) <#) $ liftIO $ do
        atomically $ writeTVar plotAreaTV pa
        return NoOp
    (NewAccuracy (name, pac)) ->
      ((s & state_item_accuracies . at name .~ Just pac) <#) $ liftIO $ do
        atomically $ do
          itemMap <- readTVar itemMapTV
          case itemMap ^. at name of
            Just fnTV ->
              do
              (item, _pac) <- readTVar fnTV
              writeTVar fnTV (item, pac)
            _ -> pure ()
        return NoOp
    (NewPlotItem (name, plotItem)) ->
      (s' <#) $ liftIO $ do
        (itemTV, isNew) <- atomically $ do
          itemMap <- readTVar itemMapTV
          case itemMap ^. at name of
            Just itemTV ->
              do
              (_item, pac) <- readTVar itemTV
              writeTVar itemTV (plotItem, pac)
              pure (itemTV, False)
            _ ->
              do
              itemTV <- newTVar (plotItem, plotAccuracy)
              writeTVar itemMapTV $ itemMap & (at name) .~ Just itemTV
              pure (itemTV, True)
        case isNew of
          True -> do
            -- start new worker thread:
            threadId <- forkIO $ enclWorker actionChan plotAreaTV itemTV name
            -- register the worker thread:
            pure $ NewWorker (name, threadId)
          False -> do
            pure NoOp
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
    (NewEnclosureSegments (name, shouldAppend, scaling, encl)) ->
      noEff $
        s & state_item_encls . at name %~ addEncl
      where
      addEncl (Just (_, oldEncl))
        | shouldAppend = Just $ (scaling, oldEncl ++ encl)
      addEncl _ = Just (scaling, encl)
    SelectItem maybeItemName -> noEff $ s & state_selectedItem .~ maybeItemName
    NoOp -> noEff s


enclWorker :: Chan Action -> TVar PlotArea -> TVar (PlotItem, PlotAccuracy) -> String -> IO ()
enclWorker actionChan plotAreaTV itemTV name =
  waitForAreaAndAct [] Nothing
  where
  waitForAreaAndAct threadIds maybePrevCompInfo =
    do
    -- wait until there is a change in the plotArea, 
    -- then work out whether the change requires reset or append:
    -- myId <- myThreadId
    -- printf "enclWorker %s: waiting\n" (show myId)
    (plotArea, plotItem, plotAccuracy, isPanned) <- atomically $ do
      pa <- readTVar plotAreaTV
      (item, pac) <- readTVar itemTV
      case maybePrevCompInfo of
        Nothing -> pure (pa, item, pac, False)
        Just (_, oldItem, oldpa, oldpac) ->
          if oldItem == item && oldpa == pa && oldpac == pac then retry
          else
            pure (pa, item, pac, oldItem == item && oldpac == pac && rect_isPanned oldpa pa)
    -- printf "enclWorker %s: updating; isPanned = %s\n" (show myId) (show isPanned)
    -- if resetting, kill any potentially active threads:
    case isPanned of
      False -> mapM_ killThread threadIds
      _ -> pure ()
    -- work over which interval to compute, if at all:
    (mdomC, dom) <-
      case (plotItem, maybePrevCompInfo) of
        (PlotItem_Curve (Curve2D dom _ _), _) ->
          pure $ if isPanned then (Nothing, dom) else (Just dom, dom)
        (PlotItem_Function _, Just (odomC, _, _, _)) | isPanned ->
          pure (get_xC_x odomC plotArea)
        _ ->
          pure (Just xP, xP)
            where
            xP = plotArea_x plotArea
    -- start a new enclosure computation thread (if needed) and recurse:
    case mdomC of
      Just domC ->
        do
        threadId <- forkIO $ sendNewEnclosureSegments plotItem isPanned plotArea plotAccuracy domC
        case isPanned of
          True -> waitForAreaAndAct (threadId : threadIds) (Just (dom, plotItem, plotArea, plotAccuracy))
          _    -> waitForAreaAndAct [threadId] (Just (dom, plotItem, plotArea, plotAccuracy))
      _ ->
        waitForAreaAndAct threadIds (Just (dom, plotItem, plotArea, plotAccuracy)) -- ie do nothing this time
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

  sendNewEnclosureSegments plotItem isPanned plotArea plotAccuracy dom =
    do
    processBatch True batch1
    mapM_ (processBatch False) batches
    where
    batch1 : batches = splitIntoBatches 100 scaledEnclosure
    processBatch isFirst batch = 
      do
      writeChan actionChan
        (NewEnclosureSegments (name, (not isFirst) || appending, scaling, batch))
      yield
    scaledEnclosure = map scaleSeg enclosure
    appending = isFunction && isPanned
    isFunction =
      case plotItem of
        PlotItem_Function _ -> True
        _ -> False
    scaling = (scalingX, scalingY)
    scaleSeg (pts, mwidth) = (map scalePt pts, mwidth)
    scalePt (x,y) = (q2d $ scalingX * x, q2d $ scalingY * y)
    scalingX = wQ/(xR-xL)
    scalingY = hQ/(yR-yL)
    Rectangle xL xR yL yR = plotArea
    enclosure =
      case plotItem of
        PlotItem_Fractal fr -> computeFractalEnclosure fr plotArea plotAccuracy
        _ -> computeEnclosure plotItem plotArea plotAccuracy dom

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
    ++ viewHeader
    ++ viewPlotAreaControls s
    ++ viewPlot s
    ++ viewAddItem s
    ++ viewItemList s
    ++ viewSelectedItemControls s
    -- ++ [br_ [], text (ms $ show $ _state_item_encls), br_ []]
    -- ++ [br_ [], text (ms $ show $ _state_plotArea), br_ []]
    -- ++ [br_ [], text (ms $ show $ _state_item_accuracies), br_ []]

instance ToMisoString Rational where
  toMisoString q = s2ms $ printf "%.4f" (q2d q)
  fromMisoString _ = error "fromMisoString not defined for Rational"

viewHeader :: [View Action]
viewHeader =
  [
    div_
    [
      Miso.style_ (Map.singleton "font-size" "32pt")
    ]
    [
      hr_ []
    , text "Exact function/curve/fractal plotter"
    ]
    , Miso.a_ [ href_ "https://github.com/michalkonecny/exact-function-plotter" ] [text "(github)"]
    , hr_ []
    ,
    text "2019, Michal Konečný, Aston University, Birmingham UK"
    , br_ []
    , text "built using "
    , Miso.a_ [ href_ "https://github.com/jensblanck/cdar" ] [ text "CDAR" ]
    , text " "
    , Miso.a_ [ href_ "https://github.com/michalkonecny/cdar/tree/mBound-noshift" ] [ text "(MK's fork)" ]
    , text ", "
    , Miso.a_ [ href_ "https://haskell-miso.org/" ] [ text "Miso" ]
    , text ", "
    , Miso.a_ [ href_ "https://www.haskell.org/" ] [ text "Haskell" ]
    , hr_ []
  ]

viewAddItem :: State -> [View Action]
viewAddItem _s@State{..} =
  [
    text "Add: "
  , flip button_ [text "function"] [ onClick (NewPlotItem (freshName "f", (PlotItem_Function RXVarX)))]
  , flip button_ [text "sin(10x^2)"] [ onClick (NewPlotItem (freshName "sin(10x^2)", (PlotItem_Function fn_sineM)))]
  , flip button_ [text "x*sin(10/x)"] [ onClick (NewPlotItem (freshName "x*sin(10/x)", (PlotItem_Function fn_sineInfty)))]
  , text "; "
  , flip button_ [text "curve"] [ onClick (NewPlotItem (freshName "c", (PlotItem_Curve defaultCurve2D)))]
  , flip button_ [text "infty"] [ onClick (NewPlotItem (freshName "infty", (PlotItem_Curve curve_infty)))]
  , flip button_ [text "mesh"] [ onClick (NewPlotItem (freshName "mesh", (PlotItem_Curve curve_mesh)))]
  , flip button_ [text "spiral"] [ onClick (NewPlotItem (freshName "spiral", (PlotItem_Curve curve_spiral)))]
  , flip button_ [text "spiralInfty"] [ onClick (NewPlotItem (freshName "spiral", (PlotItem_Curve curve_infspiral)))]
  , text "; "
  , flip button_ [text "fractal"] [ onClick (NewPlotItem (freshName "fr", (PlotItem_Fractal defaultFractal)))]
  , flip button_ [text "tree"] [ onClick (NewPlotItem (freshName "tree", (PlotItem_Fractal fractal_tree)))]
  , flip button_ [text "umbrella"] [ onClick (NewPlotItem (freshName "umbr", (PlotItem_Fractal fractal_umbrella)))]
  , br_ []
  ]
  where
  itemNames = Map.keys _state_items
  freshName prefix =
    case find (not . flip elem itemNames) $ prefix : [ prefix ++ show (i :: Int) | i <- [2..] ] of
      Just nm -> nm
      _ -> error "failed to find a default function name"

fn_sineM :: RX 
fn_sineM = s2rx "sin(10*x^2)"

fn_sineInfty :: RX
fn_sineInfty = s2rx "x*sin(10/x)"

curve_spiral :: Curve2D
curve_spiral = Curve2D (0, 50) (s2rx "0.02*x*sin(x)") (s2rx "0.02*x*cos(x)")
curve_infspiral :: Curve2D
curve_infspiral = Curve2D (0, 1) (s2rx "x*sin(1/x)") (s2rx "x*cos(1/x)")
curve_infty :: Curve2D
curve_infty = Curve2D (0, 6.29) (s2rx "0.8*sin(x)") (s2rx "0.5*sin(2*x)")
curve_mesh :: Curve2D
curve_mesh = Curve2D (0, 6.29) (s2rx "0.8*sin(5*x)") (s2rx "0.5*sin(12*x)")

fractal_tree :: AffineFractal
fractal_tree =
  AffineFractal 
    [Curve2D (0,0.5) (s2rx "0") (s2rx "x-0.5")] 
    [
      ((0.5,-0.5,-0.25),(0.5,0.5,0.25),(0,0,1))
    , ((0.5,0.5,0.25),(-0.5,0.5,0.25),(0,0,1))
    ] 
    5
    (Rectangle (-1) 1 (-0.5) 1)

fractal_umbrella :: AffineFractal
fractal_umbrella =
  AffineFractal 
    [Curve2D (0,1) (s2rx "0") (s2rx "x-1")] 
    [
      ((0.5,-0.25,-0.25),(0.5,0.25,0.25),(0,0,1))
    , ((0.5,0.25,0.25),(-0.5,0.25,0.25),(0,0,1))
    ] 
    5
    (Rectangle (-0.6) 0.6 (-1) 0.6)


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
      text $ s2ms $ printf "Fractal %s, plot depth:" itemName
    , input_ [ size_ "2", value_ (ms $ depth), onChange $ act_on_depth]
    , br_ []
    , text $ s2ms $ printf "Fractal %s, curves:" itemName
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
    AffineFractal curves _transforms depth _bounds = fractal
    viewCurve (i,curve) =
       viewEmbeddedCurveControls curve mkAction (itemName ++ "_curve" ++ show i) s
       where
       mkAction c =
        NewPlotItem (itemName, PlotItem_Fractal $ fractal & affineFractal_curves . ix (i-1) .~ c)
    act_on_depth dMS =
      case (reads $ fromMisoString dMS) of
        [(d,"")] -> NewPlotItem (itemName, PlotItem_Fractal $ fractal & affineFractal_depth .~ d)
        _ -> NoOp

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

    renderEnclosure (itemName, ((scalingX, scalingY), enclosure)) =
      map renderSegment enclosure
      where
      renderSegment (pointsPre, _) =
        polygon_  (points_ pointsMS : style) []
        where
        style =
          case _state_selectedItem of
            Just selectedName | selectedName == itemName ->
              [stroke_ "black", fill_ "#ffc0cb", fillOpacity_ "0.7"]
            _ ->
              [stroke_ "#707070", fill_ "#ffc0cb", fillOpacity_ "0.4"]
        pointsMS = ms $ intercalate " " $ map showPoint points
        showPoint (x,y) = show x ++ "," ++ show y
        -- showR :: Rational -> String
        -- showR q = show $ (fromRational q :: Double)
        transformPt (x,y) = (trX x, trY y)
        trX x
          | sameScaleX = (x - shiftX)
          | otherwise = (x - shiftX)*rescaleX
        trY y
          | sameScaleY = (q2d hQ) - (y - shiftY)
          | otherwise = (q2d hQ) - (y - shiftY)*rescaleY
        sameScaleX = (scalingX == wQ/(xR-xL))
        sameScaleY = (scalingY == - hQ/(yR-yL))
        shiftX = q2d $ xL * scalingX
        shiftY = q2d $ yL * scalingY
        rescaleX = q2d $ wQ/((xR-xL) *scalingX)
        rescaleY = q2d $ hQ/((yR-yL) *scalingY)
        points = map transformPt pointsPre

s2ms :: String -> MisoString
s2ms = ms
