-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-

  Functions given by simple symbolic expressions,
  their evaluation and automatic differentiation
  and parsing.

  Examples that plot well over [-1,1]:
  
    3*exp (2*(x-x*x*x-1))

    sin(20*x)

    (sin(1/(0.1+x^2)))

    10*x*(1-100/6*x*x*(1-100/20*x*x*(1 - 100/42*x*x*(1 - 100/72*x*x*(1-100/110*x*x)))))

-}

-- | Haskell module declaration
module Expression where

import Text.Printf
-- import Control.Lens

import Control.Applicative ((<|>))
-- import Control.Monad       (liftM, liftM2)

import qualified Text.ParserCombinators.Parsec.Expr as P
import qualified Text.ParserCombinators.Parsec      as P
-- import qualified Text.Parsec.Prim as P hiding (try)

import qualified Data.Map as Map
import Data.Ratio

import Data.CDAR


data RX =
    RXVarX
  | RXLit Rational
  | RXConst Constant
  | RXUn UnOp RX
  | RXBin BinOp RX RX
  deriving (Show, Eq, Ord)

data Constant = Pi | E
  deriving (Show, Eq, Ord)
data UnOp = Neg | PowI Integer | Sqrt | Exp | Log | Sine | Cosine | Tan
  deriving (Show, Eq, Ord)
data BinOp = Plus | Minus | Times | Divide | Power -- | Max
  deriving (Show, Eq, Ord)

showLit :: Rational -> String
showLit r 
  | isInteger = show rI
  | isDouble = show rD
  | otherwise = printf "(%s/%s)" (numerator r) (denominator r)
  where
  rI = round r
  isInteger = (fromInteger rI == r)
  rD = fromRational r :: Double
  isDouble = (toRational rD == r && read (show rD) == rD )

showRX :: String -> RX -> String
showRX varName = aux
  where
  aux RXVarX = varName
  aux (RXLit r) = showLit r
  aux (RXConst Pi) = "pi"
  aux (RXConst E) = "e"
  aux (RXUn (PowI n) rx1) =
    printf "%s^%s" (aux' rx1) (show n)
  aux (RXUn un rx1) =
    printf "%s(%s)" (showUn un) (aux rx1)
  aux (RXBin bin rx1 rx2) =
    printf "%s%s%s" (aux' rx1) (showBin bin) (aux' rx2)
  aux' rx =
    case rx of
      RXVarX -> aux rx
      RXLit _ -> aux rx
      RXConst _ -> aux rx
      _ -> "(" ++ aux rx ++ ")"

showUn :: UnOp -> String
showUn Neg = "-"
showUn (PowI n) = "^(" ++ show n ++ ")"
showUn Sqrt = "sqrt"
showUn Exp = "exp"
showUn Log = "log"
showUn Sine = "sin"
showUn Cosine = "cos"
showUn Tan = "tan"

showBin :: BinOp -> String
showBin Plus = "+"
showBin Minus = "-"
showBin Times = "*"
showBin Divide = "/"
showBin Power = "^"

foldConstants :: RX -> RX
foldConstants = aux
  where
  aux (RXUn op rx1) = 
    case (aux rx1, op) of
      (RXLit r1, Neg) -> RXLit (- r1)
      (RXLit r1, PowI n) | n >= 0 -> RXLit (r1^n)
      (rx1', _) -> RXUn op rx1'
  aux (RXBin op rx1 rx2) = 
    case (aux rx1, aux rx2, op) of
      (RXLit r1, RXLit r2, Plus) -> RXLit (r1+r2)
      (RXLit r1, RXLit r2, Minus) -> RXLit (r1-r2)
      (RXLit r1, RXLit r2, Times) -> RXLit (r1*r2)
      (RXLit r1, RXLit r2, Divide) -> RXLit (r1/r2)
      (rx1', rx2', _) -> RXBin op rx1' rx2'
  aux rx = rx

evalRX :: (CanEvalRX a) => (PrecRX a) -> RX -> a -> a
evalRX p rf0 x =
  fst $ evalCached Map.empty rf0
  where
  evalCached c rx =
    case Map.lookup rx c of
      Just res -> (res,c)
      _ ->
        evalAndCache c rx

  evalAndCache c rx =
    let
      (r, c') = evalPassCache c rx
    in
      (r, Map.insert rx r c')
  evalPassCache c (RXVarX) = (x, c)
  evalPassCache c (RXLit r) = (litRX p r, c)
  evalPassCache c (RXConst Pi) = (piRX p, c)
  evalPassCache c (RXConst E) = (eRX p, c)
  evalPassCache c (RXUn op rx1) =
    let
      (r1, c1) = evalCached c rx1
    in
      (evalOp op r1, c1)
    where
    evalOp Neg = negRX
    evalOp (PowI n) = powIRX n
    evalOp Sqrt = sqrtRX
    evalOp Exp = expRX
    evalOp Log = logRX
    evalOp Sine = sinRX
    evalOp Cosine = cosRX
    evalOp Tan = tanRX
  evalPassCache c (RXBin op rx1 rx2) =
    let
      (r1, c1) = evalCached c rx1
      (r2, c2) = evalCached c1 rx2
    in
      (evalOp op r1 r2, c2)
    where
    evalOp Plus = addRX
    evalOp Minus = subRX
    evalOp Times = mulRX
    evalOp Divide = divRX
    evalOp Power = powRX
    -- evalOp Max = maxRX

class CanEvalRX a where
  type PrecRX a
  defaultPrecRX :: a -> PrecRX a
  isIntegerRX :: a -> (Bool, Integer)
  litRX :: (PrecRX a) -> Rational -> a
  piRX :: PrecRX a -> a
  eRX :: PrecRX a -> a
  addRX :: a -> a -> a
  subRX :: a -> a -> a
  mulRX :: a -> a -> a
  divRX :: a -> a -> a
  powRX :: a -> a -> a
  powRX = powRXintExponent powRXI
  powIRX :: Integer -> a -> a
  powIRX = flip powRXI
  negRX :: a -> a
  sqrtRX :: a -> a
  expRX :: a -> a
  logRX :: a -> a
  sinRX :: a -> a
  cosRX :: a -> a
  tanRX :: a -> a

powRXintExponent :: 
  (CanEvalRX a) => 
  (a -> Integer -> a) ->
  a -> a -> a
powRXintExponent powI x y 
  | yisI = x `powI` yI
  | otherwise = powRXviaExpLog x y
  where
  (yisI, yI) = isIntegerRX y

powRXviaExpLog :: (CanEvalRX a) => a -> a -> a
powRXviaExpLog x y = 
  expRX (y `mulRX` (logRX x))

powRXI :: (CanEvalRX a) => a -> Integer -> a
powRXI xx ii
  | ii < 0 = divRX (litRX (defaultPrecRX xx) 1) (pwr (-ii))
  | otherwise = pwr ii
  where
  pwr i
    | i == 0 = litRX (defaultPrecRX xx) 1
    | otherwise = f xx i
  -- the following is taken from https://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Real.html#%5E%5E
  -- f : x0 ^ y0 = x ^ y
  f x y | even y    = f (x `mulRX` x) (y `quot` 2)
        | y == 1    = x
        | otherwise = g (x `mulRX` x) (y `quot` 2) x         -- See Note [Half of y - 1]
  -- g : x0 ^ y0 = (x ^ y) * z
  g x y z | even y = g (x `mulRX` x) (y `quot` 2) z
          | y == 1 = x `mulRX` z
          | otherwise = g (x `mulRX` x) (y `quot` 2) (x `mulRX` z) -- See Note [Half of y - 1]

instance CanEvalRX Double where
  type PrecRX Double = ()
  defaultPrecRX _ = ()
  isIntegerRX = isIntegerRX_default
  litRX _ = fromRational
  piRX _ = pi
  eRX _ = exp 1
  addRX = (+)
  subRX = (-)
  mulRX = (*)
  divRX = (/)
  powRX = powRXintExponent (^^)
  negRX = negate
  sqrtRX = sqrt
  expRX = exp
  logRX = log
  sinRX = sin
  cosRX = cos
  tanRX = tan

isIntegerRX_default :: (RealFrac a) => a -> (Bool, Integer)
isIntegerRX_default x = 
  case properFraction x of (xI, r) -> (r == 0, xI)

instance CanEvalRX CR where
  type PrecRX CR = ()
  defaultPrecRX _ = ()
  isIntegerRX = isIntegerRX_A . require 10
  litRX _ = fromRational
  piRX _ = pi
  eRX _ = exp 1
  addRX = (+)
  subRX = (-)
  mulRX = (*)
  divRX = (/)
  negRX = negate
  sqrtRX = sqrt
  expRX = exp
  logRX = log
  sinRX = sin
  cosRX = cos
  tanRX = tan

instance CanEvalRX Approx where
  type PrecRX Approx = Int
  defaultPrecRX _ = 64
  isIntegerRX = isIntegerRX_A
  litRX p = toApprox p
  piRX p = piA p
  eRX p = expA (toApprox p 1)
  addRX = (+)
  subRX = (-)
  mulRX = (*)
  divRX = (/)
  negRX = negate
  sqrtRX = sqrtA
  expRX = expA
  logRX = logA
  sinRX x = sinA x
  cosRX x = cosA x
  tanRX x = sinRX x / cosRX x

isIntegerRX_A :: Approx -> (Bool, Integer)
isIntegerRX_A (Approx _ m 0 s) 
  | s >= 0 = (True, m*(2^s))
  | otherwise = (r==0, i)
  where
  (i,r) = divMod m (2^(-s))
isIntegerRX_A _ = (False, 0)

-- higher-order automatic differentiation
data D a = D { unD :: [a] }

xD :: (CanEvalRX a) => (PrecRX a) -> a -> D a
xD p x = D $ x : litRX p 1 : repeat (litRX p 0)

instance (CanEvalRX a) => CanEvalRX (D a) where
  type PrecRX (D a) = PrecRX a
  defaultPrecRX (D (r:_)) = defaultPrecRX r
  defaultPrecRX _ = error "defaultPrecRX"
  isIntegerRX = isIntegerRX_D
  litRX p r = D $ litRX p r : repeat (litRX p 0)
  piRX p = D $ piRX p : repeat (litRX p 0)
  eRX p = D $ eRX p : repeat (litRX p 0)
  addRX (D rs1) (D rs2) = D (zipWith addRX rs1 rs2)
  subRX (D rs1) (D rs2) = D (zipWith subRX rs1 rs2)
  mulRX rr1@(D (r1:rs1)) rr2@(D (r2:rs2)) =
    D (mulRX r1 r2 : rest)
    where
    -- (f1(x)*f2(x))' = f1(x)*f2(x)' + f1(x)'*f2(x)
    (D rest) = (rr1 *.. rrd2) +.. (rrd1 *.. rr2)
    rrd1 = D rs1
    rrd2 = D rs2
    (*..) = mulRX
    (+..) = addRX
  mulRX _ _ = error "mulRX for D"
  divRX rr1@(D (r1:rs1)) rr2@(D (r2:rs2)) = D (divRX r1 r2 : rest)
    where
    -- (f1(x)/f2(x))' = (f1(x)'*f2(x) - f1(x)*f2(x)')/f2(x)^2
    (D rest) =
      ((rrd1 *..  rr2) -.. (rr1 *.. rrd2)) /.. (rr2 *.. rr2)
    rrd1 = D rs1
    rrd2 = D rs2
    (*..) = mulRX
    (-..) = subRX
    (/..) = divRX
  divRX _ _ = error "divRX for D"
  powIRX n rr1@(D (r1:rs1)) 
    | n == 0 = one
    | n == 1 = rr1
    | otherwise = D (powIRX n r1 : rest)
    where
    -- (f1(x)^n)' = n*f1'(x)*f1(x)^(n-1)
    (D rest) =
      (powIRX (n-1) rr1) *.. (nRX *.. rrd1)
    rrd1 = D rs1
    one = litRX (defaultPrecRX rr1) 1
    nRX = litRX (defaultPrecRX rr1) (fromInteger n)
    (*..) = mulRX
  powIRX _ _ = error "powIRX for D"
  powRX rr1@(D (r1:rs1)) rr2@(D (r2:rs2)) = D (powRX r1 r2 : rest)
    where
    -- (f1(x)^f2(x))' = (f1(x)^(f2(x)-1)) * (f2(x)*f1'(x)+f1(x)*f2'(x)*log(f1(x)))
    -- source: https://www.wolframalpha.com/input/?i=(f(x)%5E(g(x)))%27
    (D rest) =
      (rr1 `powRX` (rr2 -.. one))
      *..
      ((rr2 *.. rrd1) +.. (rrd2 *.. rr1 *.. (logRX rr1)))
    rrd1 = D rs1
    rrd2 = D rs2
    one = litRX (defaultPrecRX rr1) 1
    (*..) = mulRX
    (-..) = subRX
    (+..) = addRX
  powRX _ _ = error "powRX for D"
  negRX _rr1@(D rs1) = D (map negRX rs1)
  sqrtRX _rr1@(D (r1:rs1)) = res
    where
    res = D (sqrtRX r1 : rest)
    -- (sqrt(f1(x))' = f1(x)'/(2*sqrt(f1(x)))
    (D rest) = rrd1 /.. (two *.. res)
    rrd1 = D rs1
    (*..) = mulRX
    (/..) = divRX
    two = litRX (defaultPrecRX r1) 2
  sqrtRX _ = error "sqrtRX for D"
  expRX _rr1@(D (r1:rs1)) = res
    where
    res = D (expRX r1 : rest)
    -- exp(f1(x))' = f1(x)'*(exp(f1(x)))
    (D rest) = rrd1 *.. res
    rrd1 = D rs1
    (*..) = mulRX
  expRX _ = error "expRX for D"
  logRX rr1@(D (r1:rs1)) = res
    where
    res = D (logRX r1 : rest)
    -- (log(f1(x))' = f1(x)'/f1(x)
    (D rest) = rrd1 /.. rr1
    rrd1 = D rs1
    (/..) = divRX
  logRX _ = error "logRX for D"
  sinRX rr1@(D (r1:rs1)) = res
    where
    res = D (sinRX r1 : rest)
    (D rest) = rrd1 *.. (cosRX rr1)
    rrd1 = D rs1
    (*..) = mulRX
  sinRX _ = error "logRX for D"
  cosRX rr1@(D (r1:rs1)) = res
    where
    res = D (cosRX r1 : rest)
    (D rest) = negRX $ rrd1 *.. (sinRX rr1)
    rrd1 = D rs1
    (*..) = mulRX
  cosRX _ = error "logRX for D"
  tanRX r1 = sinRX r1 `divRX` (cosRX r1)


isIntegerRX_D :: (CanEvalRX a) => D a -> (Bool, Integer)
isIntegerRX_D (D (v:_)) = isIntegerRX v
isIntegerRX_D _ = (False, 0)

-- parser
-- adapted code from http://hackage.haskell.org/package/ParserFunction-0.1.0
-- by Enzo Haussecker <enzo@ucsd.edu>

-- | Parse a mathematical expression.
--
-- > >>> parse "exp(-pi*i)+1"
-- > Right (Add (Exp (Mul (Neg (Var "pi")) (Var "i"))) (Num 1.0))
--
parseRX :: String -> String -> Either P.ParseError RX
parseRX varName = P.parse expr "" . (:) '(' . flip (++) ")" . filter (/=' ')
  where
  expr :: P.Parser RX
  expr = P.buildExpressionParser table factor

  table :: [[P.Operator Char st RX]]
  table =
    -- [ [ prefix "arcsinh" ArcSinh, prefix "arcsin" ArcSin, prefix "sinh" Sinh, prefix "sin" Sin ]
    -- , [ prefix "arccosh" ArcCosh, prefix "arccos" ArcCos, prefix "cosh" Cosh, prefix "cos" Cos ]
    -- , [ prefix "arctanh" ArcTanh, prefix "arctan" ArcTan, prefix "tanh" Tanh, prefix "tan" Tan ]
    -- , [ prefix "arcsech" ArcSech, prefix "arcsec" ArcSec, prefix "sech" Sech, prefix "sec" Sec ]
    -- , [ prefix "arccsch" ArcCsch, prefix "arccsc" ArcCsc, prefix "csch" Csch, prefix "csc" Csc ]
    -- , [ prefix "arccoth" ArcCoth, prefix "arccot" ArcCot, prefix "coth" Coth, prefix "cot" Cot ]
    [ [ prefix "sin" (RXUn Sine) ]
    , [ prefix "cos" (RXUn Cosine) ]
    , [ prefix "tan" (RXUn Tan) ]
    -- , [ prefix "abs"  Abs  ]
    , [ prefix "exp"  (RXUn Exp)  ]
    , [ prefix "sqrt" (RXUn Sqrt) ]
    , [ prefix "log"  (RXUn Log)  ]
    , [ binary "^" (powerRX) P.AssocRight ]
    , [ prefix "-" (RXUn Neg) ]
    , [ binary "*" (RXBin Times) P.AssocLeft, binary "/" (RXBin Divide) P.AssocLeft ]
    , [ binary "+" (RXBin Plus) P.AssocLeft, binary "-" (RXBin Minus) P.AssocLeft ]
    ] where binary s f a = P.Infix  (       P.string s  >> return f) a
            prefix s f   = P.Prefix (P.try (P.string s) >> return f)

  powerRX rx1 rx2 = 
    case foldConstants rx2 of
      RXLit r -> 
        case properFraction r of
          (n,rm) | rm == 0 -> RXUn (PowI n) rx1
          _ -> RXBin Power rx1 rx2
      _ -> RXBin Power rx1 rx2

  factor :: P.Parser RX
  factor = do
    _    <- P.char '('
    e    <- expr
    _    <- P.char ')'
    return $! e
    <|> atom

  atom :: P.Parser RX
  atom = do
    name <- P.string varName P.<|> P.string "pi" P.<|> P.string "e"
    pure $! procName name
    <|> number
    where
    procName "pi" = RXConst Pi
    procName "e" = RXConst E
    procName name | name == varName = RXVarX
    procName name = error $ printf "%s (valid names: %s, pi, e)" name varName

  number :: P.Parser RX
  number = do
    pr <- P.many1 P.digit
    let n = foldl stl 0 pr
    P.option (RXLit n) . P.try $ do
      _  <- P.char '.'
      su <- P.many1 P.digit
      return $! RXLit $ n + foldr str 0 su
      where stl a x = (ctn x - ctn '0') + a  * 10
            str x a = (ctn x - ctn '0'  + a) / 10
            ctn     = realToFrac . fromEnum
