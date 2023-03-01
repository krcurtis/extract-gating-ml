--------------------------------------------------------------------------------
-- Copyright 2022,2023 Fred Hutchinson Cancer Center
-- Load DIVA XML file and generate a corresponding GatingML file
-- WARNING GatingML gate coordinates where the logicle transform was used in the DIVA XML are hopefully close but will not be exact

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Semigroup ((<>))
import Control.Monad (when, forM_)
import System.Environment (getArgs)
import System.Console.Docopt
import Control.DeepSeq
import Data.Maybe (isNothing, fromJust)
import qualified Data.Map as Map
import Data.List (find)
import qualified Data.Text as T

--------------------------------------------------------------------------------

import ParseDiva
import QueryDiva
import QueryDivaGates
import Diva2GatingML (convert_diva_compensation)
import Diva2Intermediate
import Intermediate2GatingML
import GatingML
import GatingML2XML


--------------------------------------------------------------------------------
-- command line argument parsing

{-
data CommandParameters = CommandParameters
    { arg_diva_file :: String
    , arg_output_file :: String
    }

command_parameters :: O.Parser CommandParameters
command_parameters = CommandParameters
     <$> O.strOption
         ( O.long "input_diva_xml"
         <> O.short 'i'
         <> O.metavar "FILE"
         <> O.help "DIVA formatted XML file" )
     <*> O.strOption
         ( O.long "output_file"
         <> O.short 'o'
         <> O.metavar "FILE"
         <> O.help "output GatingML file" )

main :: IO ()
main = O.execParser opts >>= run_app
  where
    opts = O.info (command_parameters O.<**> O.helper)
      ( O.fullDesc
     <> O.progDesc "Extract GatingML gates from DIVA XML"
     <> O.header "extract-gating-ml - app for getting gates from DIVA XML files")
-}

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- docopt command line argument parsing



patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns


find_worksheet :: DivaInfo -> String -> Maybe DivaWorksheet
find_worksheet diva_info sheet_name = find (\w -> sheet_name == dw_sheet_name w) (di_global_worksheets diva_info)
    


main = do
  args <- parseArgsOrExit patterns =<< getArgs
  --print . show $ args
  --putStrLn ""

  --let default_sheet_name = "Global Sheet1"

  when (args `isPresent` (command "summary")) $ do
    diva_file <- args `getArgOrExit` (argument "diva_xml")
    diva_info <- load_diva_info diva_file
    let unit = diva_info `deepseq` ()
    show_diva_info diva_info


  when (args `isPresent` (command "summary-comp-channels")) $ do
    diva_file <- args `getArgOrExit` (argument "diva_xml")
    diva_info <- load_diva_info diva_file
    show_all_compensated_channels diva_info


  when (args `isPresent` (command "summary-global-gates")) $ do
    diva_file <- args `getArgOrExit` (argument "diva_xml")
    diva_info <- load_diva_info diva_file
    worksheet_name <- args `getArgOrExit` (longOption "worksheet")
    
    case (find_worksheet diva_info worksheet_name) of
      Nothing -> error $ "ERROR sheet " <> worksheet_name <> " is not in DIVA XML"
      Just w -> show_hierarchy (dw_gates w)
        


  when (args `isPresent` (command "summary-tube-gates")) $ do
    diva_file <- args `getArgOrExit` (argument "diva_xml")
    diva_info <- load_diva_info diva_file
    specimen <- args `getArgOrExit` (longOption "specimen")
    tube_label <- args `getArgOrExit` (longOption "tube")    

    let diva_gates = find_specimen_tube_gates diva_info specimen tube_label
    if isNothing diva_gates
      then error $ "ERROR specimen and tube combination was not found: " <> specimen <> "/" <> tube_label
      else show_hierarchy (fromJust diva_gates)


  when (args `isPresent` (command "summary-all-gates")) $ do
    diva_file <- args `getArgOrExit` (argument "diva_xml")
    diva_info <- load_diva_info diva_file

    putStrLn "Gates from Global Worksheet:"
    forM_ (di_global_worksheets diva_info) (\sheet -> do
                                               putStrLn $ "Global Worksheet: " <> (dw_sheet_name sheet)
                                               show_hierarchy (dw_gates sheet)
                                               putStrLn "")
    putStrLn ""

    let per_specimen_tube_gates = [ (s, dt_tube_name t, dt_gates t) | (s, tubes) <- Map.toList . di_specimen_tubes $ diva_info, t <- tubes ]   
    forM_ per_specimen_tube_gates (\(s,t,g) -> do
                                                 putStrLn $ "Specimen: " <> s <> "  Tube: " <> t
                                                 show_hierarchy g
                                                 putStrLn ""
                                  )
  when (args `isPresent` (command "compare-vs-global")) $ do
    diva_file <- args `getArgOrExit` (argument "diva_xml")
    diva_info <- load_diva_info diva_file
    worksheet_name <- args `getArgOrExit` (longOption "worksheet")    
    case (find_worksheet diva_info worksheet_name) of
      Nothing -> error $ "ERROR sheet " <> worksheet_name <> " is not in DIVA XML"
      Just w -> show_comparison_with_global_worksheet w diva_info

  when (args `isPresent` (command "compare-sheets")) $ do
    diva_file <- args `getArgOrExit` (argument "diva_xml")
    diva_info <- load_diva_info diva_file
    worksheet_name <- args `getArgOrExit` (longOption "worksheet")    
    case (find_worksheet diva_info worksheet_name) of
      Nothing -> error $ "ERROR sheet " <> worksheet_name <> " is not in DIVA XML"
      Just w -> do
        putStrLn $ "Comparison of global worksheets with \"" <> worksheet_name <> "\""
        show_worksheet_comparisons w diva_info


  when (args `isPresent` (command "extract-global")) $ do
    diva_file <- args `getArgOrExit` (longOption "input_diva_xml")
    output_file <- args `getArgOrExit` (longOption "output_file")
    diva_info <- load_diva_info diva_file
    worksheet_name <- args `getArgOrExit` (longOption "worksheet")
    
    case (find_worksheet diva_info worksheet_name) of
      Nothing -> error $ "ERROR sheet " <> worksheet_name <> " is not in DIVA XML"
      Just w ->  let comp_ref = T.pack default_compensation
                     comp_matrix = convert_diva_compensation (dw_compensation_info w)
                     fluorochromes = c_fluorochromes comp_matrix
                     intermediate_gates = map (\g -> convert_diva_gate g fluorochromes) (dw_gates w)
                     (gates, transform_ref_pairs) = convert_collection intermediate_gates comp_ref
                     xml_root = gates `deepseq` to_xml transform_ref_pairs comp_matrix gates
                 in
                   xml_to_file output_file xml_root
{- remove extract-tube command for now
   USAGE:
     extract-gating-ml extract-tube -s=<slabel> -t=<tlabel> -i=<diva_xml> -o=<gatingml_output_file>
     extract-tube           Extract the gates and compensation matrix for a specific specimen and tube combination


  when (args `isPresent` (command "extract-tube")) $ do
    diva_file <- args `getArgOrExit` (longOption "input_diva_xml")
    output_file <- args `getArgOrExit` (longOption "output_file")
    specimen <- args `getArgOrExit` (longOption "specimen")
    tube_label <- args `getArgOrExit` (longOption "tube")    
    diva_info <- load_diva_info diva_file

    let diva_gates = find_specimen_tube_gates diva_info specimen tube_label
    if isNothing diva_gates
      then error $ "ERROR specimen and tube combination was not found: " <> specimen <> "/" <> tube_label
      else
        let comp_ref = T.pack default_compensation
                     comp_matrix = convert_diva_compensation (dw_compensation_info w)
                     fluorochromes = c_fluorochromes comp_matrix
                     intermediate_gates = map (\g -> convert_diva_gate g fluorochromes) (dw_gates w)
                     (gates, transform_ref_pairs) = convert_collection intermediate_gates comp_ref
                     xml_root = gates `deepseq` to_xml transform_ref_pairs comp_matrix gates
                                
              gates = map convert_diva_gate (fromJust diva_gates)
            xml_root = gates `deepseq` to_gates_only_xml gates
        in
          xml_to_file output_file xml_root
-}
