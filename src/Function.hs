{-# LANGUAGE CPP #-}
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

    10*x*(1-100/6*x*x*(1-100/20*x*x*(1 - 100/42*x*x*(1 - 100/72*x*x*(1-100/110*x*x)))))

-}

-- | Haskell module declaration
module Function where

-- import Control.Lens

import Control.Applicative ((<|>))
-- import Control.Monad       (liftM, liftM2)

import qualified Text.ParserCombinators.Parsec.Expr as P
import qualified Text.ParserCombinators.Parsec      as P
-- import qualified Text.Parsec.Prim as P hiding (try)

import qualified Data.Map as Map


import Data.CDAR


data RF =
    RFVarX
  | RFLit Rational
  | RFConst Constant
  | RFUn UnOp RF
  | RFBin BinOp RF RF
  deriving (Show, Eq, Ord)

data Constant = Pi | E
  deriving (Show, Eq, Ord)
data UnOp = Neg | PowI Integer | Sqrt | Exp | Log | Sine | Cosine | Tan
  deriving (Show, Eq, Ord)
data BinOp = Plus | Minus | Times | Divide | Power -- | Max
  deriving (Show, Eq, Ord)

foldConstants :: RF -> RF
foldConstants = aux
  where
  aux (RFUn op rf1) = 
    case (aux rf1, op) of
      (RFLit r1, Neg) -> RFLit (- r1)
      (RFLit r1, PowI n) | n >= 0 -> RFLit (r1^n)
      (rf1', _) -> RFUn op rf1'
  aux (RFBin op rf1 rf2) = 
    case (aux rf1, aux rf2, op) of
      (RFLit r1, RFLit r2, Plus) -> RFLit (r1+r2)
      (RFLit r1, RFLit r2, Minus) -> RFLit (r1-r2)
      (RFLit r1, RFLit r2, Times) -> RFLit (r1*r2)
      (RFLit r1, RFLit r2, Divide) -> RFLit (r1/r2)
      (rf1', rf2', _) -> RFBin op rf1' rf2'
  aux rf = rf

evalRF :: (CanEvalRF a) => (PrecRF a) -> RF -> a -> a
evalRF p rf0 x =
  fst $ evalCached Map.empty rf0
  where
  evalCached c rf =
    case Map.lookup rf c of
      Just res -> (res,c)
      _ ->
        evalAndCache c rf

  evalAndCache c rf =
    let
      (r, c') = evalPassCache c rf
    in
      (r, Map.insert rf r c')
  evalPassCache c (RFVarX) = (x, c)
  evalPassCache c (RFLit r) = (litRF p r, c)
  evalPassCache c (RFConst Pi) = (piRF p, c)
  evalPassCache c (RFConst E) = (eRF p, c)
  evalPassCache c (RFUn op rf1) =
    let
      (r1, c1) = evalCached c rf1
    in
      (evalOp op r1, c1)
    where
    evalOp Neg = negRF
    evalOp (PowI n) = powIRF n
    evalOp Sqrt = sqrtRF
    evalOp Exp = expRF
    evalOp Log = logRF
    evalOp Sine = sinRF
    evalOp Cosine = cosRF
    evalOp Tan = tanRF
  evalPassCache c (RFBin op rf1 rf2) =
    let
      (r1, c1) = evalCached c rf1
      (r2, c2) = evalCached c1 rf2
    in
      (evalOp op r1 r2, c2)
    where
    evalOp Plus = addRF
    evalOp Minus = subRF
    evalOp Times = mulRF
    evalOp Divide = divRF
    evalOp Power = powRF
    -- evalOp Max = maxRF

class CanEvalRF a where
  type PrecRF a
  defaultPrecRF :: a -> PrecRF a
  isIntegerRF :: a -> (Bool, Integer)
  litRF :: (PrecRF a) -> Rational -> a
  piRF :: PrecRF a -> a
  eRF :: PrecRF a -> a
  addRF :: a -> a -> a
  subRF :: a -> a -> a
  mulRF :: a -> a -> a
  divRF :: a -> a -> a
  powRF :: a -> a -> a
  powRF = powRFintExponent powRFI
  powIRF :: Integer -> a -> a
  powIRF = flip powRFI
  negRF :: a -> a
  sqrtRF :: a -> a
  expRF :: a -> a
  logRF :: a -> a
  sinRF :: a -> a
  cosRF :: a -> a
  tanRF :: a -> a

powRFintExponent :: 
  (CanEvalRF a) => 
  (a -> Integer -> a) ->
  a -> a -> a
powRFintExponent powI x y 
  | yisI = x `powI` yI
  | otherwise = powRFviaExpLog x y
  where
  (yisI, yI) = isIntegerRF y

powRFviaExpLog :: (CanEvalRF a) => a -> a -> a
powRFviaExpLog x y = 
  expRF (y `mulRF` (logRF x))

powRFI :: (CanEvalRF a) => a -> Integer -> a
powRFI xx ii
  | ii < 0 = divRF (litRF (defaultPrecRF xx) 1) (pwr (-ii))
  | otherwise = pwr ii
  where
  pwr i
    | i == 0 = litRF (defaultPrecRF xx) 1
    | otherwise = f xx i
  -- the following is taken from https://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Real.html#%5E%5E
  -- f : x0 ^ y0 = x ^ y
  f x y | even y    = f (x `mulRF` x) (y `quot` 2)
        | y == 1    = x
        | otherwise = g (x `mulRF` x) (y `quot` 2) x         -- See Note [Half of y - 1]
  -- g : x0 ^ y0 = (x ^ y) * z
  g x y z | even y = g (x `mulRF` x) (y `quot` 2) z
          | y == 1 = x `mulRF` z
          | otherwise = g (x `mulRF` x) (y `quot` 2) (x `mulRF` z) -- See Note [Half of y - 1]

instance CanEvalRF Double where
  type PrecRF Double = ()
  defaultPrecRF _ = ()
  isIntegerRF = isIntegerRF_default
  litRF _ = fromRational
  piRF _ = pi
  eRF _ = exp 1
  addRF = (+)
  subRF = (-)
  mulRF = (*)
  divRF = (/)
  powRF = powRFintExponent (^^)
  negRF = negate
  sqrtRF = sqrt
  expRF = exp
  logRF = log
  sinRF = sin
  cosRF = cos
  tanRF = tan

isIntegerRF_default :: (RealFrac a) => a -> (Bool, Integer)
isIntegerRF_default x = 
  case properFraction x of (xI, r) -> (r == 0, xI)

instance CanEvalRF CR where
  type PrecRF CR = ()
  defaultPrecRF _ = ()
  isIntegerRF = isIntegerRF_A . require 10
  litRF _ = fromRational
  piRF _ = pi
  eRF _ = exp 1
  addRF = (+)
  subRF = (-)
  mulRF = (*)
  divRF = (/)
  negRF = negate
  sqrtRF = sqrt
  expRF = exp
  logRF = log
  sinRF = sin
  cosRF = cos
  tanRF = tan

instance CanEvalRF Approx where
  type PrecRF Approx = Int
  defaultPrecRF _ = 64
  isIntegerRF = isIntegerRF_A
  litRF p = toApprox p
  piRF p = piA p
  eRF p = expA (toApprox p 1)
  addRF = (+)
  subRF = (-)
  mulRF = (*)
  divRF = (/)
  negRF = negate
  sqrtRF = sqrtA
  expRF = expA
  logRF = logA
  sinRF x = sinA (mBound x) x
  cosRF x = cosA (mBound x) x
  tanRF x = sinRF x / cosRF x

isIntegerRF_A :: Approx -> (Bool, Integer)
isIntegerRF_A (Approx _ m 0 s) 
  | s >= 0 = (True, m*(2^s))
  | otherwise = (r==0, i)
  where
  (i,r) = divMod m (2^(-s))
isIntegerRF_A _ = (False, 0)

-- higher-order automatic differentiation
data D a = D { unD :: [a] }

xD :: (CanEvalRF a) => (PrecRF a) -> a -> D a
xD p x = D $ x : litRF p 1 : repeat (litRF p 0)

instance (CanEvalRF a) => CanEvalRF (D a) where
  type PrecRF (D a) = PrecRF a
  defaultPrecRF (D (r:_)) = defaultPrecRF r
  defaultPrecRF _ = error "defaultPrecRF"
  isIntegerRF = isIntegerRF_D
  litRF p r = D $ litRF p r : repeat (litRF p 0)
  piRF p = D $ piRF p : repeat (litRF p 0)
  eRF p = D $ eRF p : repeat (litRF p 0)
  addRF (D rs1) (D rs2) = D (zipWith addRF rs1 rs2)
  subRF (D rs1) (D rs2) = D (zipWith subRF rs1 rs2)
  mulRF rr1@(D (r1:rs1)) rr2@(D (r2:rs2)) =
    D (mulRF r1 r2 : rest)
    where
    -- (f1(x)*f2(x))' = f1(x)*f2(x)' + f1(x)'*f2(x)
    (D rest) = (rr1 *.. rrd2) +.. (rrd1 *.. rr2)
    rrd1 = D rs1
    rrd2 = D rs2
    (*..) = mulRF
    (+..) = addRF
  mulRF _ _ = error "mulRF for D"
  divRF rr1@(D (r1:rs1)) rr2@(D (r2:rs2)) = D (divRF r1 r2 : rest)
    where
    -- (f1(x)/f2(x))' = (f1(x)'*f2(x) - f1(x)*f2(x)')/f2(x)^2
    (D rest) =
      ((rrd1 *..  rr2) -.. (rr1 *.. rrd2)) /.. (rr2 *.. rr2)
    rrd1 = D rs1
    rrd2 = D rs2
    (*..) = mulRF
    (-..) = subRF
    (/..) = divRF
  divRF _ _ = error "divRF for D"
  powIRF n rr1@(D (r1:rs1)) 
    | n == 0 = one
    | n == 1 = rr1
    | otherwise = D (powIRF n r1 : rest)
    where
    -- (f1(x)^n)' = n*f1'(x)*f1(x)^(n-1)
    (D rest) =
      (powIRF (n-1) rr1) *.. (nRF *.. rrd1)
    rrd1 = D rs1
    one = litRF (defaultPrecRF rr1) 1
    nRF = litRF (defaultPrecRF rr1) (fromInteger n)
    (*..) = mulRF
  powIRF _ _ = error "powIRF for D"
  powRF rr1@(D (r1:rs1)) rr2@(D (r2:rs2)) = D (powRF r1 r2 : rest)
    where
    -- (f1(x)^f2(x))' = (f1(x)^(f2(x)-1)) * (f2(x)*f1'(x)+f1(x)*f2'(x)*log(f1(x)))
    -- source: https://www.wolframalpha.com/input/?i=(f(x)%5E(g(x)))%27
    (D rest) =
      (rr1 `powRF` (rr2 -.. one))
      *..
      ((rr2 *.. rrd1) +.. (rrd2 *.. rr1 *.. (logRF rr1)))
    rrd1 = D rs1
    rrd2 = D rs2
    one = litRF (defaultPrecRF rr1) 1
    (*..) = mulRF
    (-..) = subRF
    (+..) = addRF
  powRF _ _ = error "powRF for D"
  negRF _rr1@(D rs1) = D (map negRF rs1)
  sqrtRF _rr1@(D (r1:rs1)) = res
    where
    res = D (sqrtRF r1 : rest)
    -- (sqrt(f1(x))' = f1(x)'/(2*sqrt(f1(x)))
    (D rest) = rrd1 /.. (two *.. res)
    rrd1 = D rs1
    (*..) = mulRF
    (/..) = divRF
    two = litRF (defaultPrecRF r1) 2
  sqrtRF _ = error "sqrtRF for D"
  expRF _rr1@(D (r1:rs1)) = res
    where
    res = D (expRF r1 : rest)
    -- exp(f1(x))' = f1(x)'*(exp(f1(x)))
    (D rest) = rrd1 *.. res
    rrd1 = D rs1
    (*..) = mulRF
  expRF _ = error "expRF for D"
  logRF rr1@(D (r1:rs1)) = res
    where
    res = D (logRF r1 : rest)
    -- (log(f1(x))' = f1(x)'/f1(x)
    (D rest) = rrd1 /.. rr1
    rrd1 = D rs1
    (/..) = divRF
  logRF _ = error "logRF for D"
  sinRF rr1@(D (r1:rs1)) = res
    where
    res = D (sinRF r1 : rest)
    (D rest) = rrd1 *.. (cosRF rr1)
    rrd1 = D rs1
    (*..) = mulRF
  sinRF _ = error "logRF for D"
  cosRF rr1@(D (r1:rs1)) = res
    where
    res = D (cosRF r1 : rest)
    (D rest) = negRF $ rrd1 *.. (sinRF rr1)
    rrd1 = D rs1
    (*..) = mulRF
  cosRF _ = error "logRF for D"
  tanRF r1 = sinRF r1 `divRF` (cosRF r1)


isIntegerRF_D :: (CanEvalRF a) => D a -> (Bool, Integer)
isIntegerRF_D (D (v:_)) = isIntegerRF v
isIntegerRF_D _ = (False, 0)

-- parser
-- adapted code from http://hackage.haskell.org/package/ParserFunction-0.1.0
-- by Enzo Haussecker <enzo@ucsd.edu>

-- | Parse a mathematical expression.
--
-- > >>> parse "exp(-pi*i)+1"
-- > Right (Add (Exp (Mul (Neg (Var "pi")) (Var "i"))) (Num 1.0))
--
parseRF :: String -> Either P.ParseError RF
parseRF = P.parse expr "" . (:) '(' . flip (++) ")" . filter (/=' ')
  where
  expr :: P.Parser RF
  expr = P.buildExpressionParser table factor

  table :: [[P.Operator Char st RF]]
  table =
    -- [ [ prefix "arcsinh" ArcSinh, prefix "arcsin" ArcSin, prefix "sinh" Sinh, prefix "sin" Sin ]
    -- , [ prefix "arccosh" ArcCosh, prefix "arccos" ArcCos, prefix "cosh" Cosh, prefix "cos" Cos ]
    -- , [ prefix "arctanh" ArcTanh, prefix "arctan" ArcTan, prefix "tanh" Tanh, prefix "tan" Tan ]
    -- , [ prefix "arcsech" ArcSech, prefix "arcsec" ArcSec, prefix "sech" Sech, prefix "sec" Sec ]
    -- , [ prefix "arccsch" ArcCsch, prefix "arccsc" ArcCsc, prefix "csch" Csch, prefix "csc" Csc ]
    -- , [ prefix "arccoth" ArcCoth, prefix "arccot" ArcCot, prefix "coth" Coth, prefix "cot" Cot ]
    [ [ prefix "sin" (RFUn Sine) ]
    , [ prefix "cos" (RFUn Cosine) ]
    , [ prefix "tan" (RFUn Tan) ]
    -- , [ prefix "abs"  Abs  ]
    , [ prefix "exp"  (RFUn Exp)  ]
    , [ prefix "sqrt" (RFUn Sqrt) ]
    , [ prefix "log"  (RFUn Log)  ]
    , [ binary "^" (powerRF) P.AssocRight ]
    , [ prefix "-" (RFUn Neg) ]
    , [ binary "*" (RFBin Times) P.AssocLeft, binary "/" (RFBin Divide) P.AssocLeft ]
    , [ binary "+" (RFBin Plus) P.AssocLeft, binary "-" (RFBin Minus) P.AssocLeft ]
    ] where binary s f a = P.Infix  (       P.string s  >> return f) a
            prefix s f   = P.Prefix (P.try (P.string s) >> return f)

  powerRF rf1 rf2 = 
    case foldConstants rf2 of
      RFLit r -> 
        case properFraction r of
          (n,rm) | rm == 0 -> RFUn (PowI n) rf1
          _ -> RFBin Power rf1 rf2
      _ -> RFBin Power rf1 rf2

  factor :: P.Parser RF
  factor = do
    _    <- P.char '('
    e    <- expr
    _    <- P.char ')'
    return $! e
    <|> atom

  atom :: P.Parser RF
  atom = do
    name <- P.string "x" P.<|> P.string "pi" P.<|> P.string "e"
    pure $! procName name
    <|> number
    where
    procName "x" = RFVarX
    procName "pi" = RFConst Pi
    procName "e" = RFConst E
    procName name = error $ name ++ " (valid names: x, pi, e)"

  number :: P.Parser RF
  number = do
    pr <- P.many1 P.digit
    let n = foldl stl 0 pr
    P.option (RFLit n) . P.try $ do
      _  <- P.char '.'
      su <- P.many1 P.digit
      return $! RFLit $ n + foldr str 0 su
      where stl a x = (ctn x - ctn '0') + a  * 10
            str x a = (ctn x - ctn '0'  + a) / 10
            ctn     = realToFrac . fromEnum
