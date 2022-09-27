--------------------------------------------------------------------------------
--- Copyright 2022 Fred Hutchinson Cancer Center
--- Convert Diva Gates to GatingML Gates

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Diva2GatingML where


import qualified Data.Map.Strict as Map
import qualified Data.List as L
import Numeric.AD.Newton.Double
--import Numeric.AD.Internal.Type
--import Numeric.AD.Rank1.Forward.Double
import Numeric.AD.Double

import qualified Data.Text as T

--------------------------------------------------------------------------------
import GatingML
import ParseDiva


-- or use Either String [Gate] ??
convert :: DivaInfo -> String -> String -> [Gate]
convert DivaInfo{..} specimen tube = results
  where
    results = case (Map.lookup specimen di_specimen_tubes) of
                Nothing -> error $ "ERROR specimen " <> specimen <> " not found in Diva info"
                Just tubes -> case (L.find (\t -> tube == dt_tube_name t) tubes) of
                                Nothing -> error $ "ERROR tube " <> tube <> " was not found for specimen " <> specimen <> " in Diva info"
                                Just tube -> extract_gates_from_tube_info tube


extract_gates_from_tube_info :: DivaTube -> [Gate]
extract_gates_from_tube_info DivaTube{..} = map convert_diva_gate dt_gates


diva_parent_to_id :: T.Text -> Maybe T.Text
diva_parent_to_id diva_parent_text = case (L.last . T.splitOn "\\" $ diva_parent_text) of
                                       "All Events" -> Nothing
                                       x -> Just x


convert_diva_gate :: DivaGate -> Gate
convert_diva_gate DivaGate{..} | (r_type dg_region) == RectangleRegion = RectangleGate{..}
  where
    rg_id = dg_name
    rg_parent_id = diva_parent_to_id dg_parent

    [dx_min, dx_max, dy_min, dy_max] = let points = case (length (r_points dg_region) == 4) of
                                                  False -> error $ "Rectangle Region for " <> T.unpack dg_name <> " has incorrect number of vertices"
                                                  True -> r_points dg_region
                                           x_points = map fst points
                                           y_points = map snd points
                                       in [ head . L.sort $ x_points, last . L.sort $ x_points, head . L.sort $ y_points, last . L.sort $ y_points]
    x_fluorescent = r_xparam dg_region
    y_fluorescent = r_yparam dg_region

    x_transform = transform_coord dg_x_scaled dg_x_log dg_x_scale
    y_transform = transform_coord dg_y_scaled dg_y_log dg_y_scale

    x_min = x_transform dx_min
    x_max = x_transform dx_max
    y_min = y_transform dy_min
    y_max = y_transform dy_max
                                      
    rg_x_dim = GatingDimension Nothing Nothing (Just x_min) (Just x_max) x_fluorescent
    rg_y_dim = GatingDimension Nothing Nothing (Just y_min) (Just y_max) y_fluorescent

convert_diva_gate DivaGate{..} | (r_type dg_region) == PolygonRegion = PolygonGate{..}
  where
    pg_id = dg_name
    pg_parent_id = diva_parent_to_id dg_parent

    x_fluorescent = r_xparam dg_region
    y_fluorescent = r_yparam dg_region

    pg_x_dim = GatingDimension Nothing Nothing Nothing Nothing x_fluorescent
    pg_y_dim = GatingDimension Nothing Nothing Nothing Nothing y_fluorescent

    d_x_points = map fst (r_points dg_region)
    d_y_points = map snd (r_points dg_region)    

    x_transform = transform_coord dg_x_scaled dg_x_log dg_x_scale
    y_transform = transform_coord dg_y_scaled dg_y_log dg_y_scale
    
    pg_points = zip (map x_transform d_x_points) (map y_transform d_y_points)

convert_diva_gate DivaGate{..} | otherwise = error $ "ERROR region type " <> show (r_type dg_region) <> " not handled"


-- what about setting compensation reference?

log10 :: Double -> Double
log10 x = log x  / (log 10)

-- transform from Diva gate coordinates to untransformed ADC values, expected to range between 0 and 2**18, but are sometimes negative
transform_coord :: Bool -> Bool -> Double -> Double -> Double
transform_coord False False scale_factor = id
transform_coord False True scale_factor = (**) 10  -- diva seems to apply plain log10

 -- I think this case is the biexponential transform, not sure what the biexponential parameters truly are
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

 -- I think this case is the biexponential transform, not sure what the biexponential parameters truly are
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




{-
{-# LANGUAGE FlexibleContexts #-}
test_w_fit_func x = w_fit_func x
  where
    paramW = 2.1
    log10' x = log x  / (log 10)    
    w_fit_func x = auto paramW - 2 * x' * (log10' x') / (x' + 1)
      where
        x' = abs(x)
-}



