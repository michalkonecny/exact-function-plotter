module CanvasPlotter 
(
  Context
, getContext
, clearCanvas
, drawEnclosure
)
where

-- empty stubs:

data Context = Context

getContext :: IO Context
getContext = pure Context

drawEnclosure :: Context -> (Bool, [[(Double,Double)]]) -> IO ()
drawEnclosure _ _ = pure ()

clearCanvas :: Context -> (Double, Double) -> IO ()
clearCanvas _ _ = pure ()
