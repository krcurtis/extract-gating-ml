--------------------------------------------------------------------------------
--- Copyright 2022,2023 Fred Hutchinson Cancer Center
--- Convert Diva information on transform and gating coordinates to linear space

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module DivaTransform where


import qualified Data.Map.Strict as Map
import qualified Data.List as L
import Numeric.AD.Newton.Double
import Numeric.AD.Double
import qualified Numeric.LinearAlgebra as NLA


--------------------------------------------------------------------------------


log10 :: Double -> Double
log10 x = log x  / (log 10)

-- transform from Diva gate coordinates to untransformed ADC values, expected to range between 0 and 2**18, but are sometimes negative
transform_coord :: Bool -> Bool -> Double -> Double -> Double
transform_coord False False scale_factor = id
transform_coord False True scale_factor = (**) 10  -- diva seems to apply plain log10

 -- From fitting various conversions with FlowJo, this case is apparently the biexponential transform. Parameters are based on educated guess and fitting
transform_coord True True 0.0 = \x -> let y = x/s
                                               in case (y >= paramW + paramA) of
                                                    True -> paramT * 10 ** (-(paramM-paramW-paramA)) * ( 10**(y-paramW-paramA) - p**2 * 10**(-(y-paramW-paramA)/p) + p**2  - 1.0)
                                                    False -> -paramT * 10 ** (-(paramM-paramW-paramA)) * ( 10**(paramW+paramA-y) - p**2 * 10**(-(paramW+paramA-y)/p) + p**2  - 1.0)
  where
    paramT = 2**18 :: Double
    paramM = 4.5
    paramA = 0.0
    paramW = 0.0
    p = 1.0 -- or p = 0
    s = 910.2346083636803  -- from some attempts at fitting

 -- From fitting various conversions with FlowJo, this case is apparently the biexponential transform. Parameters are based on educated guess and fitting  
transform_coord True True scale_factor = \x -> let y = x/s
                                               in case (y >= paramW + paramA) of
                                                    True -> paramT * 10 ** (-(paramM-paramW-paramA)) * ( 10**(y-paramW-paramA) - p**2 * 10**(-(y-paramW-paramA)/p) + p**2  - 1.0)
                                                    False -> -paramT * 10 ** (-(paramM-paramW-paramA)) * ( 10**(paramW+paramA-y) - p**2 * 10**(-(paramW+paramA-y)/p) + p**2  - 1.0)

  where
    paramT = 2**18 :: Double
    paramM = 4.5
    paramA = 0.0
    r = scale_factor

    paramW :: Double
    paramW = 0.5 * (paramM - log10 (paramT /abs(r)))
    s = 910.2346083636803  -- from some attempts at fitting
    
    log10' x = log x  / (log 10)
    w_fit_func x = auto paramW - 2 * x' * (log10' x') / (x' + 1)
      where
        x' = abs(x)
    
    p = last . take 30 $ (findZero w_fit_func 1)




