--------------------------------------------------------------------------------
--- Copyright 2022,2023 Fred Hutchinson Cancer Center
--- Convert Diva Gates to GatingML Gates

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}



module Diva2GatingML where


import qualified Data.Map.Strict as Map
import qualified Data.List as L
import qualified Numeric.LinearAlgebra as NLA

import qualified Data.Text as T

--------------------------------------------------------------------------------
import GatingML
import ParseDiva
import DivaTransform

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




{-
-- this version assumes the matrix-already-inverted flag can be used
convert_diva_compensation :: [ (String, [Double])] -> Compensation
convert_diva_compensation info_rows = Compensation{..}
  where
    c_fluorochromes = map (T.pack . fst) info_rows
    c_spectrum_rows = map snd info_rows
-}

--apply a matrix inverse
convert_diva_compensation :: [ (String, [Double])] -> Compensation
convert_diva_compensation info_rows = Compensation{..}
  where
    n = length info_rows    
    c_fluorochromes = map (T.pack . fst) info_rows
    rows = map snd info_rows    

    m = NLA.matrix n (concat rows)
    m' = NLA.tr . NLA.inv $ m
    row_vectors = NLA.toRows m'
    c_spectrum_rows = map NLA.toList row_vectors








