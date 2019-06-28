module CanvasPlotter 
(
  Context
, getContext
, clearCanvas
, drawGridLine
, drawText
, drawEnclosure
, drawRootEncl
)
where

import Data.String

-- import Control.Lens as Lens hiding (view)

-- Javascript Canvas
import JavaScript.Web.Canvas hiding (Context, getContext)
import qualified JavaScript.Web.Canvas as Canvas

type Context = Canvas.Context

getContext :: IO Context
getContext = getCtx

clearCanvas :: Context -> (Double, Double) -> IO ()
clearCanvas ctx (h,w) = do
  clearRect 0 0 h w ctx

drawGridLine :: Context -> (Double, Double) -> (Double, Double) -> IO ()
drawGridLine ctx (x1,y1) (x2,y2) =
  do
  save ctx
  beginPath ctx
  moveTo x1 y1 ctx
  lineTo x2 y2 ctx
  -- setLineDash [1,3] ctx -- should be supported in newer ghcjs
  strokeStyle 0 0 0 0.3 ctx
  stroke ctx
  restore ctx

drawText :: Context -> (Double, Double) -> String -> Double -> IO ()
drawText ctx (x,y) t size =
  do
  save ctx
  flip font ctx $ fromString $ show size ++ "px Arial"
  fillStyle 0 0 0 1 ctx
  fillText (fromString t) x y ctx
  restore ctx

drawEnclosure :: Context -> (Bool, [[(Double,Double)]]) -> IO ()
drawEnclosure ctx (isSelected, polygons) =
  do
  save ctx
  setStyle
  mapM_ (drawPolygon ctx) polygons
  restore ctx
  where
  setStyle
    | isSelected =
      do
      fillStyle 255 192 203 0.7 ctx
      strokeStyle 0 0 0 1 ctx
    | otherwise =
      do
      fillStyle 255 192 2013 0.4 ctx
      strokeStyle 0 0 0 0.7 ctx

drawPolygon :: Context -> [(Double, Double)] -> IO ()
drawPolygon ctx ((x1,y1):points) = do
  beginPath ctx
  moveTo x1 y1 ctx
  mapM_ (\(xi,yi) -> lineTo xi yi ctx) points
  lineTo x1 y1 ctx
  fill ctx
  stroke ctx
drawPolygon ctx [] = pure ()

type RootEnclosure t = ((t,t), (Maybe Int, Maybe Int)) -- location of some number of roots

drawRootEncl :: Context -> Double -> RootEnclosure Double -> IO ()
drawRootEncl ctx yZero rootEncl =
  do
  save ctx
  beginPath ctx
  moveTo l yZero ctx
  lineTo r yZero ctx
  lineWidth 5 ctx
  lineCap LineCapRound ctx
  strokeStyle 255 0 0 1 ctx
  stroke ctx
  restore ctx
  where
  ((l,r), (_, _)) = rootEncl

foreign import javascript unsafe "$r = document.getElementById('canvas').getContext('2d');"
  getCtx :: IO Canvas.Context
