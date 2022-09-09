--------------------------------------------------------------------------------
--- Copyright 2022 Fred Hutchinson Cancer Center
--- Pull out various bits of information from DivaInfo record

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}


module GateComparison where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (catMaybes, fromJust, isJust, fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as L

import Numeric.LinearAlgebra.Data


import ParseDiva

--------------------------------------------------------------------------------

data GateComparison = GateComparison { gc_matches :: Int -- number of gates that have the same name and match exactly
                                     , gc_mismatches :: Int -- number of gates with the same name that don't match exactly
                                     , gc_missing :: Int -- number of gates that are not in the target and expected to be there from the reference
                                     , gc_extra :: Int -- number of gates that are in the target but not the reference
                                     }
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------



compare_gate_lists :: [DivaGate] -> [DivaGate] -> GateComparison
compare_gate_lists ref target = GateComparison{..}
  where
    ref_names = Set.fromList . map dg_name $ ref
    target_names = Set.fromList . map dg_name $ target
    gc_missing = Set.size (Set.difference ref_names target_names)
    gc_extra   = Set.size (Set.difference target_names ref_names)

    common_names = Set.toList (Set.intersection ref_names target_names)
    ref_group    = Map.fromList [ (dg_name g, g) | g <- ref]    
    target_group = Map.fromList [ (dg_name g, g) | g <- target]

    common_named_gates = map snd . Map.toList . Map.intersectionWith (\x y -> (x,y)) ref_group $ target_group
    gc_matches = length [ (g,g') | (g, g') <- common_named_gates, g == g']
    gc_mismatches = length [ (g,g') | (g, g') <- common_named_gates, g /= g']    
    




compare_compensation_matrices :: [ (String, [Double])] -> [ (String, [Double])] -> Either String ()
compare_compensation_matrices ref target = results
  where
    threshold = 1e-5
    ref_names = map fst ref
    target_names = map fst target
    
    results = case (ref_names == target_names) of
                False -> Left "Parameter names do not match"
                True -> let ref_rows      = map snd ref
                            target_rows   = map snd target
                            ref_matrix    = matrix (length ref_rows) (concat ref_rows)
                            target_matrix = matrix (length target_rows) (concat target_rows)
                            dif           = abs (ref_matrix - target_matrix)
                            is_dif       = any (\x -> x > threshold) (concat . toLists $ dif)
                        in  case is_dif of
                              False -> Right ()
                              True -> Left $ "Difference in some values larger than " ++ show threshold
  

{-

rows = 
matrix (length rows)  (concat rows)

-}
