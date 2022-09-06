--------------------------------------------------------------------------------
--- Copyright 2022 Fred Hutchinson Cancer Center
--- Pull out various bits of information from DivaInfo record

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}


module QueryDiva where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (catMaybes, fromJust, isJust, fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as L

import ParseDiva

import PrettyPrintTable

--------------------------------------------------------------------------------

show_tube_gates :: DivaInfo -> IO ()
show_tube_gates DivaInfo{..} = do
  let records = [ (s, dt_tube_name t, length . dt_gates $ t ) | (s, tubes) <- Map.toList di_specimen_tubes, t <- tubes ]
  mapM_ (\(s,t, g) -> putStrLn $ "Specimen: " <> s <> "  Tube: " <> t <> "  n_gates: " <> (show g)) records

show_tube_compensation_summary :: DivaInfo -> IO ()
show_tube_compensation_summary DivaInfo{..} = do
  let records = [ (s, dt_tube_name t, length . dt_compensation_info $ t) | (s, tubes) <- Map.toList di_specimen_tubes, t <- tubes ]
  mapM_ (\(s,t, c) -> putStrLn $ "Specimen: " <> s <> "  Tube: " <> t <> "  Number parameters with compensation: " <> (show c)) records

show_tube_info :: DivaInfo -> IO ()
show_tube_info DivaInfo{..} = do
  let records = [ (s, dt_tube_name t, length . dt_gates $ t, length . dt_compensation_info $ t) | (s, tubes) <- Map.toList di_specimen_tubes, t <- tubes ]
  mapM_ (\(s,t,g,c) -> putStrLn $ "Specimen: " <> s <> "  Tube: " <> t <> "  n_gates: " <> (show g) <> "  compensation params: " <> (show c)) records

show_diva_info :: DivaInfo -> IO ()
show_diva_info DivaInfo{..} = do
  putStrLn $ "Global Worksheet n_gates: " <> (show . length $ di_global_worksheet_gates) <> "  compensation params: " <> (show . length $ di_global_worksheet_compensation_info )
  let records = [ (s, dt_tube_name t, show . length . dt_gates $ t, show . length . dt_compensation_info $ t) | (s, tubes) <- Map.toList di_specimen_tubes, t <- tubes ]

  display_column4_table ("Specimen", "Tube", "n_gates", "n_compensation_params") records
  --putStrLn $ L.intercalate "\t" ["Specimen", "Tube", "n_gates", "n_compensation_params"]
  -- mapM_ (\(s,t,g,c) -> putStrLn $ L.intercalate "\t" [s, t, show g, show c]) records


find_specimen_tube_gates :: DivaInfo -> String -> String -> Maybe [DivaGate]
find_specimen_tube_gates DivaInfo{..} specimen tube_label = do
    tubes <- Map.lookup specimen di_specimen_tubes
    tube <- L.find (\t -> tube_label == dt_tube_name t) tubes
    return . dt_gates $ tube

              


find_gate_with_name :: [DivaGate] -> String -> Maybe DivaGate
find_gate_with_name gates gate_name = L.find (\g -> text_name == dg_name g) gates
  where
    text_name = T.pack gate_name


find_all_gates_with_same_name :: String -> DivaInfo -> [(String, DivaGate)]
find_all_gates_with_same_name gate_name DivaInfo{..} = global_gate ++ tube_gates
  where
    global_gate = case (find_gate_with_name di_global_worksheet_gates gate_name) of
                    Nothing -> []
                    Just g -> [("Global worksheet", g)]
                    
    find_tube_gates :: (String, DivaTube) -> Maybe (String, DivaGate)
    find_tube_gates (specimen,tube) = do
      gate <- find_gate_with_name (dt_gates tube) gate_name
      return (specimen <> "/" <> (dt_tube_name $ tube), gate)
    tube_gates = catMaybes . map find_tube_gates $ [ (s,t) | (s, tubes) <- Map.toList di_specimen_tubes, t <- tubes]
    

show_all_compensated_channels :: DivaInfo -> IO ()
show_all_compensated_channels DivaInfo{..} = do
  putStrLn $ "Global Worksheet compensated channels: " <> (L.intercalate ","  (map fst di_global_worksheet_compensation_info))
  --putStrLn $ L.intercalate "\t" ["Specimen", "Tube", "compensation_channels"]
  --mapM_ (\(s,t,cs) -> putStrLn $ L.intercalate "\t" [s, t, L.intercalate "," cs]) records
  let records = [ (s, dt_tube_name t, L.intercalate "," . map fst . dt_compensation_info $ t) | (s, tubes) <- Map.toList di_specimen_tubes, t <- tubes ]

  display_column3_table ("Specimen", "Tube", "compensation_channels") records



{-
show_gates_with_same_name :: DivaInfo -> String -> IO ()
-}


