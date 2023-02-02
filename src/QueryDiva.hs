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

import Control.Monad (forM)

--------------------------------------------------------------------------------

import ParseDiva
import PrettyPrintTable
import GateComparison


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


{-
show_worksheet_info :: DivaWorksheet -> IO ()
show_worksheet_info DivaWorksheet{..} = do
  putStrLn $ "Global Worksheet " <> (show dw_sheet_name) <> " n_gates: " <> (show . length $ dw_gates) <> "  compensation params: " <> (show . length $ dw_compensation_info )

  let records = [ (s, dt_tube_name t, show . length . dt_gates $ t, show . length . dt_compensation_info $ t) | (s, tubes) <- Map.toList di_specimen_tubes, t <- tubes ]
-}

show_worksheet_table :: [DivaWorksheet] -> IO ()
show_worksheet_table sheets = do
  let records = [ (dw_sheet_name w, show . length . dw_gates $ w, show . length . dw_compensation_info $ w) | w <- sheets]
  display_column3_table ("Sheet", "n_gates", "n_compensation_params") records


show_diva_info :: DivaInfo -> IO ()
show_diva_info DivaInfo{..} = do
  putStrLn $ "Global Worksheets"
  show_worksheet_table di_global_worksheets
  putStrLn ""

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
find_all_gates_with_same_name gate_name DivaInfo{..} = global_gates ++ tube_gates
  where
    find_gate_in_worksheet :: DivaWorksheet -> Maybe (String, DivaGate)
    find_gate_in_worksheet sheet = do
      gate <- find_gate_with_name (dw_gates sheet) gate_name
      return (dw_sheet_name sheet, gate)

    global_gates = catMaybes . map (find_gate_in_worksheet) $  di_global_worksheets

    find_tube_gates :: (String, DivaTube) -> Maybe (String, DivaGate)
    find_tube_gates (specimen,tube) = do
      gate <- find_gate_with_name (dt_gates tube) gate_name
      return (specimen <> "/" <> (dt_tube_name $ tube), gate)
    tube_gates = catMaybes . map find_tube_gates $ [ (s,t) | (s, tubes) <- Map.toList di_specimen_tubes, t <- tubes]


show_all_compensated_channels :: DivaInfo -> IO ()
show_all_compensated_channels DivaInfo{..} = do
  putStrLn $ "Compensation channels in Global Worksheets"
  let sheet_records = [ (dw_sheet_name w, L.intercalate ","  . map fst . dw_compensation_info $ w) | w <- di_global_worksheets]
  display_column2_table ("Worksheet", "compensation_channels") sheet_records

  putStrLn ""
  putStrLn "Compensation channels in specimen & tubes"
  let records = [ (s, dt_tube_name t, L.intercalate "," . map fst . dt_compensation_info $ t) | (s, tubes) <- Map.toList di_specimen_tubes, t <- tubes ]

  display_column3_table ("Specimen", "Tube", "compensation_channels") records




summarize_gate_compensation_comparison :: [DivaGate] -> [(String, [Double])] -> [DivaGate] -> [ (String, [Double])] -> [String]
summarize_gate_compensation_comparison ref_gates ref_comp target_gates target_comp = results
  where
    GateComparison{..} = compare_gate_lists ref_gates target_gates
    comp_info = compare_compensation_matrices ref_comp target_comp
    comp_msg = case comp_info of
                 Left msg -> msg
                 Right _ -> "Matched"
    results = [comp_msg, show gc_matches, show gc_mismatches, show gc_missing, show gc_extra]

show_comparison_with_global_worksheet :: DivaWorksheet -> DivaInfo -> IO ()
show_comparison_with_global_worksheet DivaWorksheet{..} DivaInfo{..} = do
  let ref_gates = dw_gates
      ref_comp = dw_compensation_info
      comp_gates = [ (s, dt_tube_name t, dt_compensation_info t, dt_gates t) | (s, tubes) <- Map.toList di_specimen_tubes, t <- tubes ]
      compare_them (specimen_name, tube_name, comp, gates) = [specimen_name, tube_name] ++ summarize_gate_compensation_comparison ref_gates ref_comp gates comp
      rows = map compare_them comp_gates
  display_many_column_table ["Specimen", "Tube", "compensation", "n_gate_matches", "n_gate_mismatches", "n_gate_missing", "n_gate_extra"] rows


show_worksheet_comparisons :: DivaWorksheet -> DivaInfo -> IO ()
show_worksheet_comparisons ref_sheet DivaInfo{..} = do
  let ref_gates = dw_gates ref_sheet
      ref_comp = dw_compensation_info ref_sheet
      comp_gates = [ (dw_sheet_name w, dw_compensation_info w, dw_gates w) | w <- di_global_worksheets]
      compare_them (sheet_name, comp, gates) = [sheet_name] ++ summarize_gate_compensation_comparison ref_gates ref_comp gates comp
      rows = map compare_them comp_gates
  display_many_column_table ["Worksheet", "compensation", "n_gate_matches", "n_gate_mismatches", "n_gate_missing", "n_gate_extra"] rows
  
{-
show_gates_with_same_name :: DivaInfo -> String -> IO ()
-}
