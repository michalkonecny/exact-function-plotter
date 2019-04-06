{-# LANGUAGE CPP #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Haskell module declaration
module Function where

-- import Control.Lens

import Control.Applicative ((<|>))
-- import Control.Monad       (liftM, liftM2)

import qualified Text.ParserCombinators.Parsec.Expr as P
import qualified Text.ParserCombinators.Parsec      as P
import qualified Text.Parsec.Prim as P hiding (try)

import qualified Data.Map as Map


import Data.CDAR -- hiding (showCR)

data RF =
    RFVarX
  | RFLit Rational
  | RFConst Constant
  | RFUn UnOp RF
  | RFBin BinOp RF RF
  deriving (Show, Eq, Ord)

data Constant = Pi | E
  deriving (Show, Eq, Ord)
data UnOp = Neg | Sqrt | Exp | Log | Sine | Cosine | Tan
  deriving (Show, Eq, Ord)
data BinOp = Plus | Minus | Times | Divide | Power -- | Max
  deriving (Show, Eq, Ord)

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
  litRF :: (PrecRF a) -> Rational -> a
  piRF :: PrecRF a -> a
  eRF :: PrecRF a -> a
  addRF :: a -> a -> a
  subRF :: a -> a -> a
  mulRF :: a -> a -> a
  divRF :: a -> a -> a
  powRF :: a -> a -> a
  negRF :: a -> a
  sqrtRF :: a -> a
  expRF :: a -> a
  logRF :: a -> a
  sinRF :: a -> a
  cosRF :: a -> a
  tanRF :: a -> a

instance CanEvalRF Double where
  type PrecRF Double = ()
  defaultPrecRF _ = ()
  litRF _ = fromRational
  piRF _ = pi
  eRF _ = exp 1
  addRF = (+)
  subRF = (-)
  mulRF = (*)
  divRF = (/)
  powRF r1 r2 = exp (r2 * (log r1))
  negRF = negate
  sqrtRF = sqrt
  expRF = exp
  logRF = log
  sinRF = sin
  cosRF = cos
  tanRF = tan

instance CanEvalRF CR where
  type PrecRF CR = ()
  defaultPrecRF _ = ()
  litRF _ = fromRational
  piRF _ = pi
  eRF _ = exp 1
  addRF = (+)
  subRF = (-)
  mulRF = (*)
  divRF = (/)
  powRF r1 r2 = exp (r2 * (log r1))
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
  litRF p = toApprox p
  piRF p = piA p
  eRF p = expA (toApprox p 1)
  addRF = (+)
  subRF = (-)
  mulRF = (*)
  divRF = (/)
  powRF r1 r2 = expA (r2 * (logA r1))
  negRF = negate
  sqrtRF = sqrtA
  expRF = expA
  logRF = logA
  -- sinRF = sinA
  -- cosRF = cosA
  -- tanRF = tanA

-- higher-order automatic differentiation
data D a = D { unD :: [a] }

xD :: (CanEvalRF a) => (PrecRF a) -> a -> D a
xD p x = D $ x : litRF p 1 : repeat (litRF p 0)

instance (CanEvalRF a) => CanEvalRF (D a) where
  type PrecRF (D a) = PrecRF a
  defaultPrecRF (D (r:_)) = defaultPrecRF r
  defaultPrecRF _ = error "defaultPrecRF"
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
  powRF r1 r2 = expRF (r2 `mulRF` (logRF r1))
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
    , [ binary "^" (RFBin Power) P.AssocRight ]
    , [ prefix "-" (RFUn Neg) ]
    , [ binary "*" (RFBin Times) P.AssocLeft, binary "/" (RFBin Divide) P.AssocLeft ]
    , [ binary "+" (RFBin Plus) P.AssocLeft, binary "-" (RFBin Minus) P.AssocLeft ]
    ] where binary s f a = P.Infix  (       P.string s  >> return f) a
            prefix s f   = P.Prefix (P.try (P.string s) >> return f)

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
