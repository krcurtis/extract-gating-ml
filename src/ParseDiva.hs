--------------------------------------------------------------------------------
--- Parse FACSDiva xml gating file for conversion to Gating-ML 2.0

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ParseDiva where





import Text.XML.Light
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (catMaybes)



{-

Basic xml structure when specimens are listed in XML, Version 6.1.3

bdfacs
  experiment
    instrument_settings
  specimen...
  specimen ...
    keywords
    tube
      lasers
      keywords -- various key-value parameters like a configuration
      labels -- links surface markers to florescent dyes
      instrument_settings  -- this appears in several places ???
        parameter -- lots of stuff
          compensation -- What to do with compensation?
      gates

TODO link gates for each speciment
  <tube name="FITC Stained Control">
  <data_filename>124483.fcs</data_filename>



TODO Or look for      <acquisition_worksheets name="Global Worksheets">
  /bdfacs/experiment/acquisition_worksheets/worksheet_template

GATES TODO
  "AND_Classifier", "OR_Classifier", "NOT_Classifier")

TODO Transforms, compensation stuff

there are vthings about biexp_scale in the instrument settings section ...

Under experiment, theris is a "log_decades" text field, relevant?

-}

--------------------------------------------------------------------------------


data RegionType = RectangleRegion | PolygonRegion | IntervalRegion | UnknownRegion
  deriving (Show, Eq)

data Region = Region
                { r_type :: RegionType
                , r_xparam :: T.Text
                , r_yparam :: T.Text
                , r_points :: [(Double, Double)]
                }
  deriving (Show, Eq)


data DivaGate = DivaGate
            { dg_name :: T.Text
            , dg_enabled :: Bool
            , dg_parent :: T.Text
            , dg_region :: Region
            , dg_x_scaled :: Bool
            , dg_y_scaled :: Bool
            , dg_x_log :: Bool
            , dg_y_log :: Bool
            , dg_x_scale :: Double
            , dg_y_scale :: Double
            , dg_input :: T.Text -- same as parent?
            }
  deriving (Show, Eq)
            
type DivaGateSet = [DivaGate]

--------------------------------------------------------------------------------

diva_file = "/home/kcurtis/project-base/software/apps/extract-gating-ml/testdata/PE_2.xml"

simple_name s = QName s Nothing Nothing

convert = T.pack . strContent

convert_to_bool x = case strContent x of
                      "true" -> True
                      "false" -> False
                      _ -> error $ "ERROR could not covert " ++ (show . strContent $ x) ++ " to boolean"
                      
convert_to = read . strContent



gate_contents :: IO ()
gate_contents = do
    source <- T.readFile diva_file
    let contents = parseXML source
        entries   = concatMap (findElements $ simple_name "bdfacs") (onlyElems contents)
        --symbols  = map (findAttr $ simpleName "Symbol") entries
        --item = from_xml_ ...  (head entries)
    --print . head $ entries
    if 1 == length entries
      then print . show $ (gates_in_specimens (entries !! 0)) !! 0
      else print "ERROR expecting bdfacs root xml node"


gates_in_specimens :: Element -> [DivaGateSet]
gates_in_specimens root_node = map collect_gate_set specimen_elements
  where
    specimen_elements = findElements (simple_name "specimen")  root_node


get_gates_element :: Element -> Element
get_gates_element base_node = case length gates_elements of
                                1 -> gates_elements !! 0
                                0 -> error "ERROR no gates element found"
                                _ -> error "ERROR unexpected number of gates elements found"
  where
    gates_elements = findElements (simple_name "gates")  base_node

collect_gate_set :: Element -> DivaGateSet
collect_gate_set base_node = catMaybes (map parse_gate gate_elements)
  where
    gate_elements = findElements (simple_name "gate")  base_node




parse_gate :: Element -> Maybe DivaGate
parse_gate g = do
    dg_name <- fmap convert $ findElement (simple_name "name") g
    dg_enabled <- fmap convert_to_bool $ findElement (simple_name "enabled") g
    dg_parent <- fmap convert $ findElement (simple_name "parent") g
    Just dg_region <- fmap parse_region $ findElement (simple_name "region") g
    dg_x_scaled <- fmap convert_to_bool $ findElement (simple_name "is_x_parameter_scaled") g
    dg_y_scaled <- fmap convert_to_bool $ findElement (simple_name "is_y_parameter_scaled") g
    dg_x_log    <- fmap convert_to_bool $ findElement (simple_name "is_x_parameter_log") g
    dg_y_log    <- fmap convert_to_bool $ findElement (simple_name "is_y_parameter_log") g
    dg_x_scale  <- fmap convert_to $ findElement (simple_name "x_parameter_scale_value") g
    dg_y_scale  <- fmap convert_to $ findElement (simple_name "y_parameter_scale_value") g
    dg_input    <- fmap convert $ findElement (simple_name "input") g
    return DivaGate{..}


parse_region_type :: String -> RegionType
parse_region_type s = case s of
                  "POLYGON_REGION" -> PolygonRegion
                  "RECTANGLE_REGION" -> RectangleRegion
                  "INTERVAL_REGION" -> IntervalRegion
                  "BINNER_REGION" -> UnknownRegion
                  _ -> error $ "Unknown region string " ++ s


parse_region :: Element -> Maybe Region
parse_region r = do
  r_xparam <- fmap T.pack $ findAttr (simple_name "xparm") r
  r_type <- fmap parse_region_type $ findAttr (simple_name "type") r
  r_yparam <- if IntervalRegion == r_type
              then (pure "")
              else fmap T.pack $ findAttr (simple_name "yparm") r
  r_points <- fmap parse_points (findElement (simple_name "points") r)
  return Region{..}


parse_points :: Element -> [(Double, Double)]
parse_points p = points'
  where
    points = findElements (simple_name "point") p
    points' = catMaybes $ map parse_point points




parse_point :: Element -> Maybe (Double,Double)
parse_point p = do
  x <- fmap read $ findAttr (simple_name "x") p
  y <- fmap read $ findAttr (simple_name "y") p
  return (x,y)
