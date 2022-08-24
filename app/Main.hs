--------------------------------------------------------------------------------
-- Copyright 2022 Fred Hutchinson Cancer Center
-- Load DIVA XML file and generate a corresponding GatingML file
-- WARNING GatingML gate coordinates where the logicle transform was used in the DIVA XML are hopefully close but will not be exact

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Semigroup ((<>))
import Control.Monad (when)
import System.Environment (getArgs)
import System.Console.Docopt
import Control.DeepSeq
import Data.Maybe (isNothing, fromJust)

--------------------------------------------------------------------------------

import ParseDiva
import QueryDiva
import Diva2GatingML
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

main = do
  args <- parseArgsOrExit patterns =<< getArgs
  --print . show $ args
  --putStrLn ""


  when (args `isPresent` (command "summary")) $ do
    --print . show $ args
    diva_file <- args `getArgOrExit` (longOption "input_diva_xml")
    diva_info <- load_diva_info diva_file
    show_diva_info diva_info


  when (args `isPresent` (command "summary-comp-channels")) $ do
    diva_file <- args `getArgOrExit` (longOption "input_diva_xml")
    diva_info <- load_diva_info diva_file
    show_all_compensated_channels diva_info


  when (args `isPresent` (command "summary-global-gates")) $ do
    diva_file <- args `getArgOrExit` (longOption "input_diva_xml")
    diva_info <- load_diva_info diva_file
    show_hierarchy (di_global_worksheet_gates diva_info)


  when (args `isPresent` (command "summary-tube-gates")) $ do
    diva_file <- args `getArgOrExit` (longOption "input_diva_xml")
    diva_info <- load_diva_info diva_file
    specimen <- args `getArgOrExit` (longOption "specimen")
    tube_label <- args `getArgOrExit` (longOption "tube")    

    let diva_gates = find_specimen_tube_gates diva_info specimen tube_label
    if isNothing diva_gates
      then error $ "ERROR specimen and tube combination was not found: " <> specimen <> "/" <> tube_label
      else show_hierarchy (fromJust diva_gates)


  when (args `isPresent` (command "extract-global")) $ do
    diva_file <- args `getArgOrExit` (longOption "input_diva_xml")
    output_file <- args `getArgOrExit` (longOption "output_file")
    diva_info <- load_diva_info diva_file

    let gates = map convert_diva_gate (di_global_worksheet_gates diva_info)
        xml_root = gates `deepseq` to_xml gates
    xml_to_file output_file xml_root


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
        let gates = map convert_diva_gate (fromJust diva_gates)
            xml_root = gates `deepseq` to_xml gates
        in
          xml_to_file output_file xml_root
