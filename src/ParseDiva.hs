--------------------------------------------------------------------------------
--- Copyright 2022 Keith Curtis
--- Copyright 2022 Fred Hutchinson Cancer Center
--- Parse FACSDiva xml gating file for conversion to Gating-ML 2.0
        


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module ParseDiva where




import GHC.Generics
import Control.DeepSeq

import Text.XML.Light
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (catMaybes, fromJust, isJust, fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as L

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

Gates can also be linked to multiple nodes under
  acquisition_worksheets name="Global Worksheets"
  /bdfacs/experiment/acquisition_worksheets/worksheet_template


TODO link fcs filenames for tubes
  <tube name="FITC Stained Control">
  <data_filename>124483.fcs</data_filename>


GATES TODO
  "AND_Classifier", "OR_Classifier", "NOT_Classifier")

there are things about biexp_scale in the instrument settings section, relevant?

Under experiment, there is is a "log_decades" text field, relevant?

-}

--------------------------------------------------------------------------------


data RegionType = RectangleRegion | PolygonRegion | IntervalRegion | UnknownRegion
  deriving (Show, Eq, Generic, NFData)

data Region = Region
                { r_type :: RegionType
                , r_xparam :: T.Text
                , r_yparam :: T.Text
                , r_points :: [(Double, Double)]
                }
  deriving (Show, Eq, Generic, NFData)


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
  deriving (Show, Eq, Generic, NFData)
            
type DivaGateSet = [DivaGate]



data DivaTube = DivaTube { dt_tube_name :: String
                         , dt_gates :: [DivaGate]
                         , dt_has_compensation :: Bool
                         , dt_compensation_info :: [ (String, [Double])]
                         }
                deriving (Show, Generic, NFData)

data DivaWorksheet = DivaWorksheet { dw_sheet_name :: String
                                   , dw_gates :: [DivaGate]
                                   , dw_compensation_info :: [ (String, [Double])]
                                   }
  deriving (Show, Generic, NFData)

data DivaInfo = DivaInfo { di_specimens :: [String]  -- named specimens, there might be specimen sections without names, which are not included
                         , di_specimen_tubes :: Map.Map String [DivaTube]
                         -- what about tube name to DivaTube, this might make sense if tube names don't overlap ...
                         , di_has_overlap :: Bool  -- some tube names overlap
                         , di_overlapping_tube_names :: [String]  --list of tube names that occur in more than one specimen
                         , di_tube_names :: [String] -- unique listing of the tube names
                         , di_global_worksheets :: [DivaWorksheet]
                         }
  deriving (Show, Generic, NFData)


--------------------------------------------------------------------------------



simple_name s = QName s Nothing Nothing

convert = T.pack . strContent

convert_to_bool x = case strContent x of
                      "true" -> True
                      "false" -> False
                      _ -> error $ "ERROR could not covert " ++ (show . strContent $ x) ++ " to boolean"
                      
convert_to = read . strContent




load_root_node :: String -> IO Element
load_root_node filename = do
    source <- T.readFile filename
    let contents = parseXML source
        entries   = concatMap (findElements $ simple_name "bdfacs") (onlyElems contents)
        
    if length entries == 1
    then return $ entries !! 0
    else error $ "ERROR could not load bdfacs root node from" <> filename


{-
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
-}


specimen_nodes :: Element -> [Element]
specimen_nodes root_node = findElements (simple_name "specimen")  root_node

specimen_listing :: Element -> [String]
specimen_listing root_node = catMaybes . map (findAttr (simple_name "name")) $ nodes
  where
    nodes = specimen_nodes root_node

specimen_mapping :: Element -> Map.Map String Element
specimen_mapping root_node = Map.fromList specimens'
  where
    specimens = [ (findAttr (simple_name "name") s, s) | s <- specimen_nodes root_node]
    specimens' = [ (fromJust name, s) | (name, s) <- specimens, isJust name]



{-
gates_in_specimens :: Element -> [DivaGateSet]
gates_in_specimens root_node = map collect_gate_set specimen_elements
  where
    specimen_elements = findElements (simple_name "specimen")  root_node
-}

gates_in_specimen :: Element -> String -> Maybe DivaGateSet
gates_in_specimen root_node specimen_name = fmap collect_gate_set s
  where
    mapping = specimen_mapping root_node
    s = Map.lookup specimen_name mapping
    


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

collect_from_gates_element :: Element -> [DivaGate]
collect_from_gates_element node = collect_gate_set gates_node
  where
    gates_node = get_gates_element node




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



find_global_acquistion_node :: Element -> Maybe Element
find_global_acquistion_node root = findElement (simple_name "acquisition_worksheets") root


-- assumes given a parameter node
single_parameter_compensation :: Element -> Maybe (String, [Double])
single_parameter_compensation node = do
  parameter_name <- findAttr (simple_name "name") node
  compensation_node <- findElement (simple_name "compensation") node
  
  let coeff_nodes = findElements (simple_name "compensation_coefficient") compensation_node
      coeffs = fmap convert_to coeff_nodes
  return (parameter_name, coeffs)
  

-- assumes given a tube node
compensation_info :: Element -> Maybe [ (String, [Double])]
compensation_info node = do
    instrument_settings <- findElement (simple_name "instrument_settings") node
    let parameter_nodes = findElements (simple_name "parameter") node
    return $ catMaybes $ map single_parameter_compensation parameter_nodes


parse_diva_tube :: Element -> DivaTube
parse_diva_tube tube_node = DivaTube{..}
  where
    dt_tube_name = case (findAttr (simple_name "name") tube_node) of
                     Nothing -> error "ERROR no tube name attribute in node"
                     Just x -> x
    dt_gates = collect_gate_set tube_node
    (dt_has_compensation, dt_compensation_info) = case (compensation_info tube_node) of
                                                    Nothing -> (False, [])
                                                    Just xs -> (True, xs)
    

collect_tube_info :: Element -> [DivaTube]
collect_tube_info node = map parse_diva_tube nodes
  where
    nodes = findElements (simple_name "tube")  node 


find_duplicates :: (Ord a) => (Set.Set a) -> (Set.Set a) -> [a] -> Set.Set a
find_duplicates so_far dups [] = dups
find_duplicates so_far dups (x:xs) | Set.member x so_far = find_duplicates so_far (Set.insert x dups) xs
find_duplicates so_far dups (x:xs) | otherwise = find_duplicates (Set.insert x so_far) dups xs

parse_worksheet :: Element -> DivaWorksheet
parse_worksheet worksheet_node = DivaWorksheet{..}
  where
    dw_sheet_name = case (findAttr (simple_name "name") worksheet_node) of
                     Nothing -> error "ERROR no name attribute in worksheet"
                     Just x -> x
    dw_gates = collect_gate_set worksheet_node
    dw_compensation_info = fromMaybe [] . compensation_info $ worksheet_node
    dw_has_compensation = length (dw_compensation_info) > 0                                                    
    dw_gate_info = collect_from_gates_element worksheet_node
    

collect_worksheets :: Element -> [DivaWorksheet]
collect_worksheets root = results
  where
    -- todo check has name "Global Worksheets"
    results = case find_global_acquistion_node root of
                Nothing -> []
                Just acquisition_node -> let template_nodes = findElements (simple_name "worksheet_template") acquisition_node
                                             worksheets = map parse_worksheet template_nodes
                                         in
                                           worksheets




parse_diva_xml :: Element -> DivaInfo
parse_diva_xml root = DivaInfo{..}
  where
    smap = specimen_mapping root
    di_specimens = map fst $ Map.toList smap
    di_specimen_tubes = Map.fromList [ (s, collect_tube_info e) | (s,e) <- Map.toList smap]
    all_tubes = (map dt_tube_name . concat . map snd . Map.toList ) di_specimen_tubes
    di_overlapping_tube_names = Set.toList $ find_duplicates Set.empty Set.empty all_tubes
    di_has_overlap = length di_overlapping_tube_names > 0
    di_tube_names = Set.toList . Set.fromList $ all_tubes
    di_global_worksheets = collect_worksheets root


load_diva_info :: String -> IO DivaInfo
load_diva_info filename = do
    root <- load_root_node filename
    --putStrLn "Loaded root node"
    
    return $ parse_diva_xml root


