--------------------------------------------------------------------------------
--- Copyright 2023 Fred Hutchinson Cancer Center
--- Implement Logicle transform as in Moor and Parks 2012 and GatingML 2.0 Spec

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Logicle where


import qualified Data.Map.Strict as Map
import qualified Data.List as L


import Numeric.AD.Newton.Double
import Numeric.AD.Double
import Numeric.AD.Mode.Forward.Double

import qualified Numeric.LinearAlgebra as NLA

import qualified Data.Text as T


--------------------------------------------------------------------------------


compute_d :: Double -> Double -> Double
compute_d w b = d
  where
    d_fit_func :: AD s ForwardDouble -> AD s ForwardDouble
    d_fit_func xx = (auto 2.0) * (log xx  - log (auto b)) + (auto w) * (xx + (auto b))
    d = last . take 50 $ (findZero d_fit_func 1)


compute_x :: Double -> Double
compute_x offset = result
  where
    x_fit_func :: AD s ForwardDouble -> AD s ForwardDouble
    x_fit_func xx = xx^2 - (auto offset)
    result = last . take 10 $ findZero (x_fit_func) 1


logicle :: Double -> Double -> Double -> Double -> Double -> Double
logicle x paramT paramW paramM paramA = y
  where
    w = paramW / (paramM + paramA)
    x2 = paramA / (paramM + paramA)
    x1 = x2 + w
    x0 = x2 + 2.0 * w
    b = (paramM + paramA) * log 10.0

    d = compute_d w b

    ca = exp (x0 * (b+d))
    fa = exp(b*x1) - ca / exp (d*x1)
    
    a = paramT / (exp b - fa - (ca / exp d))
    c = ca * a
    f = fa * a
    
    b_fit_func :: AD s ForwardDouble -> AD s ForwardDouble
    b_fit_func yy = (auto a) * exp ((auto b) * yy) - (auto c) * exp (auto (-d) * yy) - (auto f) - (auto x)
    
    y = last . take 100 $ (findZero b_fit_func 0.5)
    --y = take 100 $ (findZero b_fit_func 0.5)
  

biexponential ::  Double -> Double -> Double -> Double -> Double -> Double
biexponential y paramT paramW paramM paramA = result
  where
    w = paramW / (paramM + paramA)
    x2 = paramA / (paramM + paramA)
    x1 = x2 + w
    x0 = x2 + 2.0 * w
    b = (paramM + paramA) * log 10.0

    d = compute_d w b

    ca = exp (x0 * (b+d))
    fa = exp(b*x1) - ca / exp (d*x1)

    a = paramT / (exp b - fa - (ca / exp d))
    c = ca * a
    f = fa * a

    result = a * log (b*y) - c * exp ((-d)*y) - f
