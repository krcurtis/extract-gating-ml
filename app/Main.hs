--------------------------------------------------------------------------------
-- Copyright 2022 Fred Hutchinson Cancer Center
-- Load DIVA XML file and generate a corresponding GatingML file
-- WARNING GatingML gate coordinates where the logicle transform was used in the DIVA XML are hopefully close but will not be exact


module Main where

import Options.Applicative
import qualified Options.Applicative as O
import Data.Semigroup ((<>))



--------------------------------------------------------------------------------
-- command line argument parsing

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


--------------------------------------------------------------------------------


run_app :: CommandParameters -> IO ()
run_app = undefined -- TODO
