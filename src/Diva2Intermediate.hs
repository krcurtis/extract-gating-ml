--------------------------------------------------------------------------------
--- Copyright 2023 Fred Hutchinson Cancer Center
--- Convert Diva Gates to an intermediate gate structures

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}

module Diva2Intermediate where


import qualified Data.Map.Strict as Map
import qualified Data.List as L
import qualified Numeric.LinearAlgebra as NLA

import qualified Data.Text as T

--------------------------------------------------------------------------------
import IntermediateGate
import ParseDiva
import DivaTransform
import Logicle


diva_parent_to_id :: T.Text -> Maybe T.Text
diva_parent_to_id diva_parent_text = case (L.last . T.splitOn "\\" $ diva_parent_text) of
                                       "All Events" -> Nothing
                                       x -> Just x


suggest_transform :: Bool -> Bool -> Double -> Transform
suggest_transform  False False _ = Linear
suggest_transform  False True _ = Log paramT paramM
  where
    paramT = 2.0**18
    paramM = 4.5
suggest_transform  True True 0.0 = Log paramT paramM
  where
    paramT = 2.0**18
    paramM = 4.5
suggest_transform  True True scale_factor = Biexponential paramT paramM paramW paramA
  where
    paramT = 2.0**18
    paramM = 4.5
    paramW = 0.5 * (paramM - log10 (paramT /abs(scale_factor)))
    paramA = 0.0

suggest_transform  True False _ = error "ERROR unexpected case for scaled=True, log=False"


retransform_coord :: Transform -> Double -> Double
retransform_coord Linear x = x
retransform_coord (Log paramT paramM) x = 1.0 + (1.0/paramM) * log10 (x/paramT)  -- GatingML parametrized logarithmic transformation flog
retransform_coord (Biexponential paramT paramW paramM paramA) x = logicle x paramT paramW paramM paramA -- GatingML Logicle


convert_diva_gate :: DivaGate -> [T.Text] -> IntermediateGate
convert_diva_gate DivaGate{..} fluorochromes | (r_type dg_region) == RectangleRegion = BasicRectangleGate{..}
  where

    brg_id = dg_name
    brg_parent_id = diva_parent_to_id dg_parent

    brg_x_channel = r_xparam dg_region
    brg_y_channel = r_yparam dg_region

    brg_x_compensated = brg_x_channel `elem` fluorochromes
    brg_y_compensated = brg_y_channel `elem` fluorochromes

    brg_x_transform = suggest_transform dg_x_scaled dg_x_log dg_x_scale
    brg_y_transform = suggest_transform dg_y_scaled dg_y_log dg_y_scale

    [dx_min, dx_max, dy_min, dy_max] = let points = case (length (r_points dg_region) == 4) of
                                                  False -> error $ "Rectangle Region for " <> T.unpack dg_name <> " has incorrect number of vertices"
                                                  True -> r_points dg_region
                                           x_points = map fst points
                                           y_points = map snd points
                                       in [ head . L.sort $ x_points, last . L.sort $ x_points, head . L.sort $ y_points, last . L.sort $ y_points]

    x_transform = transform_coord dg_x_scaled dg_x_log dg_x_scale
    y_transform = transform_coord dg_y_scaled dg_y_log dg_y_scale

    linear_x_min = x_transform dx_min
    linear_x_max = x_transform dx_max
    linear_y_min = y_transform dy_min
    linear_y_max = y_transform dy_max


    brg_x_range = (retransform_coord brg_x_transform linear_x_min, retransform_coord brg_x_transform linear_x_max)
    brg_y_range = (retransform_coord brg_y_transform linear_y_min, retransform_coord brg_y_transform linear_y_max)

convert_diva_gate DivaGate{..} fluorochromes | (r_type dg_region) == PolygonRegion = BasicPolygonGate{..}
  where
    bpg_id = dg_name
    bpg_parent_id = diva_parent_to_id dg_parent

    bpg_x_channel = r_xparam dg_region
    bpg_y_channel = r_yparam dg_region

    bpg_x_compensated = bpg_x_channel `elem` fluorochromes
    bpg_y_compensated = bpg_y_channel `elem` fluorochromes

    bpg_x_transform = suggest_transform dg_x_scaled dg_x_log dg_x_scale
    bpg_y_transform = suggest_transform dg_y_scaled dg_y_log dg_y_scale


    raw_x_points = map fst (r_points dg_region)
    raw_y_points = map snd (r_points dg_region)

    x_transform = transform_coord dg_x_scaled dg_x_log dg_x_scale
    y_transform = transform_coord dg_y_scaled dg_y_log dg_y_scale

    linear_x_points = map x_transform raw_x_points
    linear_y_points = map y_transform raw_y_points

    bpg_vertices = zip (map (retransform_coord bpg_x_transform) linear_x_points) (map (retransform_coord bpg_y_transform) linear_y_points)


convert_diva_gate DivaGate{..} _ | otherwise = error $ "ERROR region type " <> show (r_type dg_region) <> " not handled"

