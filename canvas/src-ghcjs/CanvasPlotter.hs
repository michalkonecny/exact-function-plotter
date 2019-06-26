module CanvasPlotter 
(
  Context
, getContext
, clearCanvas
, drawEnclosure
)
where

-- import Control.Lens as Lens hiding (view)

-- Javascript Canvas
import JavaScript.Web.Canvas hiding (Context, getContext)
import qualified JavaScript.Web.Canvas as Canvas

type Context = Canvas.Context

getContext :: IO Context
getContext = getCtx

drawEnclosure :: Context -> (Bool, [[(Double,Double)]]) -> IO ()
drawEnclosure ctx (isSelected, polygons) =
  do
  setStyle
  mapM_ (drawPolygon ctx) polygons
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

clearCanvas :: Context -> (Double, Double) -> IO ()
clearCanvas ctx (h,w) = do
  clearRect 0 0 h w ctx

foreign import javascript unsafe "$r = document.getElementById('canvas').getContext('2d');"
  getCtx :: IO Canvas.Context

foreign import javascript unsafe "$1.globalCompositeOperation = 'destination-over';"
  setGlobalCompositeOperation :: Canvas.Context -> IO ()
