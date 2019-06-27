module CanvasPlotter 
(
  Context
, getContext
, clearCanvas
, drawGridLine
, drawText
, drawEnclosure
)
where

-- empty stubs:

data Context = Context

getContext :: IO Context
getContext = pure Context

clearCanvas :: Context -> (Double, Double) -> IO ()
clearCanvas _ _ = pure ()

drawGridLine :: Context -> (Double, Double) -> (Double, Double) -> IO ()
drawGridLine _ _ _ = pure ()

drawText :: Context -> (Double, Double) -> String -> Double -> IO ()
drawText _ _ _ _ = pure ()

drawEnclosure :: Context -> (Bool, [[(Double,Double)]]) -> IO ()
drawEnclosure _ _ = pure ()
