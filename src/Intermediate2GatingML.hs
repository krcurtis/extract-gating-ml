--------------------------------------------------------------------------------
--- Copyright 2023 Fred Hutchinson Cancer Center
--- Convert Intermediate Gates to GatingML Gates

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}



module Intermediate2GatingML where


import qualified Data.Map.Strict as Map
import qualified Data.List as L

import qualified Data.Text as T

--------------------------------------------------------------------------------
import GatingML
import IntermediateGate



convert_collection :: [IntermediateGate] -> T.Text -> [Gate]
convert_collection intermediate_gates compensation_ref = gates
  where
    gates = undefined


convert_intermediate_gate :: IntermediateGate -> (Maybe T.Text) -> (Maybe T.Text) -> T.Text -> Gate
convert_intermediate_gate BasicRectangleGate{..} x_transform_ref y_transform_ref compensation_ref = undefined -- RectangleGate{..}
{-
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
-}
convert_intermediate_gate BasicPolygonGate{..} x_transform_ref y_transform_ref compensation_ref = undefined -- PolygonGate{..}
{-
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
-}



