--------------------------------------------------------------------------------
--- Copyright 2023 Fred Hutchinson Cancer Center
--- Convert Intermediate Gates to GatingML Gates

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}



module Intermediate2GatingML where


import qualified Data.Map.Strict as Map
import qualified Data.List as L
import qualified Data.Set as Set

import qualified Data.Text as T

--------------------------------------------------------------------------------
import GatingML
import IntermediateGate


intermediate_x_transform :: IntermediateGate -> Transform
intermediate_x_transform BasicRectangleGate{..} = brg_x_transform
intermediate_x_transform BasicPolygonGate{..} = bpg_x_transform

intermediate_y_transform :: IntermediateGate -> Transform
intermediate_y_transform BasicRectangleGate{..} = brg_y_transform
intermediate_y_transform BasicPolygonGate{..} = bpg_y_transform



convert_collection :: [IntermediateGate] -> T.Text -> ([Gate], [(Transform, T.Text)])
convert_collection intermediate_gates compensation_ref = (gates, Map.toList transform_map)
  where
    all_transforms = [ intermediate_x_transform g | g <- intermediate_gates ] ++ [ intermediate_y_transform g | g <- intermediate_gates ]
    log_transforms = L.nub (filter is_log_transform all_transforms)
    logicle_transforms = L.nub (filter is_logicle_transform all_transforms)  -- want to preserve order

    named_log_transforms = [ (t, T.pack ("Log" ++ show i)) | (t,i) <- zip log_transforms [1..]]
    named_logicle_transforms = [ (t, T.pack ("Logicle" ++ show i)) | (t,i) <- zip logicle_transforms [1..]]
    transform_map = Map.fromList $ named_log_transforms ++ named_logicle_transforms
    gates = map (\g -> convert_intermediate_gate g transform_map compensation_ref) intermediate_gates



convert_intermediate_gate :: IntermediateGate -> (Map.Map Transform T.Text) -> T.Text -> Gate
convert_intermediate_gate BasicRectangleGate{..} transform_map compensation_ref = RectangleGate{..}
  where
    rg_id = brg_id
    rg_parent_id = brg_parent_id

    (x_min, x_max) = brg_x_range
    (y_min, y_max) = brg_y_range
                                      
    rg_x_dim = GatingDimension { gd_compensation_ref = if brg_x_compensated then Just compensation_ref else Nothing
                               , gd_transformation_ref = Map.lookup brg_x_transform transform_map
                               , gd_minimum = (Just x_min)
                               , gd_maximum = (Just x_max)
                               , gd_name = brg_x_channel }
               
    rg_y_dim = GatingDimension { gd_compensation_ref = if brg_y_compensated then Just compensation_ref else Nothing
                               , gd_transformation_ref = Map.lookup brg_y_transform transform_map
                               , gd_minimum = (Just y_min)
                               , gd_maximum = (Just y_max)
                               , gd_name = brg_y_channel }

convert_intermediate_gate BasicPolygonGate{..} transform_map compensation_ref = PolygonGate{..}
  where
    pg_id = bpg_id
    pg_parent_id = bpg_parent_id

    pg_x_dim = GatingDimension { gd_compensation_ref = if bpg_x_compensated then Just compensation_ref else Nothing
                               , gd_transformation_ref = Map.lookup bpg_x_transform transform_map
                               , gd_minimum = Nothing
                               , gd_maximum = Nothing
                               , gd_name = bpg_x_channel }
               
    pg_y_dim = GatingDimension { gd_compensation_ref = if bpg_y_compensated then Just compensation_ref else Nothing
                               , gd_transformation_ref = Map.lookup bpg_y_transform transform_map
                               , gd_minimum = Nothing
                               , gd_maximum = Nothing
                               , gd_name = bpg_y_channel }

    pg_points = bpg_vertices




